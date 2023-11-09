const {cd, cat, pwd, ls, echo, mkdir, ShellString} = require('shelljs');
const cheerio = require('cheerio');
const path = require('path');

const scriptPath = __dirname;

const docPath = '../doc/';
const outDirPath = '../simple-doc/';

const tsdKindPrefix = 'tsd-kind-';

const dropStr = n => str => str.slice(n);
const tailStr = dropStr(1);
const pipe = (x, ...fs) => fs.reduce((acc, f) => f(acc), x);
const groupBy = (f) => (xs) => xs.reduce((acc, x) => {
    const key = f(x);
    acc[key] = acc[key] ?? [];
    acc[key].push(x);
    return acc;
}, {});

// workaround for ESM-only support of ventojs
let vto;

const processUrl = url => {
    if (!url) return {href: null, debugInfo: 'null url'};

    const regex = /^(?:\.\.\/([a-zA-Z.]+)\/)?([a-zA-Z_.]+)\.html(?:#(.+))?/;
    const match = url.match(regex);

    if (match) {
        const [_, category, fileName, memberName] = match;
        const debugInfo = `category: <<${category}>>, fileName: <<${fileName}>>, memberName: <<${memberName}>>`;
        if (category === 'classes' && !memberName) return {href:`..`, debugInfo: 'class | ' + debugInfo};
        const fileNameStartsWithUpperCase = fileName && fileName?.[0] === fileName?.[0].toUpperCase();
        if (!category && !memberName && fileNameStartsWithUpperCase) return {href:`..`, debugInfo: 'probably class | ' + debugInfo};
        return {
            href:`../${!memberName ? 'Root' : fileName}/${memberName ?? fileName}.html`,
            debugInfo: 'main return | ' + debugInfo
        };
    }
    const isLinkToLibSources = url.includes('monnef/ts-opt');
    if (isLinkToLibSources) return {href: url, debugInfo: 'link to lib sources'};
    return {href: '..', debugInfo: 'unknown url'};
};

const processMemberHtml = ($, memberEl, options) => {
    memberEl.attr('data-className', options.className);

    memberEl.find('a').each((i, rEl) => {
        const el = $(rEl);
        const href = el.attr('href');

        el.attr('data-origHref', href);
        const {href: newHref, debugInfo} = processUrl(href);
        el.attr('href', newHref);
        el.attr('data-debugInfo', debugInfo);
    });

    // remove copy to clipboard button (source code, e.g. examples)
    memberEl.find('button').filter((_, rEl) => $(rEl).text() === 'Copy').remove();

    return memberEl;
};

const ITEM_KINDS = {
    member: 'member',
    class: 'class'
}

const ownText = x => x.contents().filter(function () { return this.type === 'text'; }).text();

const sectionNameToType = sectionName => {
    if (!sectionName) return undefined;
    switch (sectionName) {
        case 'Properties':
            return 'property';
        case 'Methods':
            return 'method';
        case 'Accessors':
            return 'accessor';
        case 'Constructors':
            return 'constructor';
        default:
            throw new Error(`Unknown section name: ${sectionName}`);
    }
}

const parsePageTitle = el => {
    const txt = el.find('.tsd-page-title h1').text();
    const [rawType, name] = txt.split(' ', 2);
    return [rawType?.toLowerCase(), name];
}

const processFile = (inputFilePath, options = {className: null}) => {
    // console.log('Processing', inputFilePath);
    const html = cat(inputFilePath).toString();
    const $ = cheerio.load(html);

    const members = $('.tsd-panel').map((i, rEl) => {
        const el = $(rEl);

        const [nameFromPageTitle, typeFromPageTitle] = parsePageTitle(el);
        const name = el.find('a.tsd-anchor, li.tsd-anchor-link').prop('id') ?? nameFromPageTitle;

        if (!name) {
            const firstText = el.children().first().text();
            const expectedFromText = ['Type Parameters', 'Hierarchy'].includes(firstText);
            const expectedFromClass = ['tsd-index-panel', 'tsd-comment'].some(cls => el.hasClass(cls));
            if (!expectedFromText && !expectedFromClass) console.log('no name for member in', inputFilePath, '\n* firstText:',JSON.stringify(firstText), '\n* html:\n', $.html(el), '\n\n');
            return;
        }

        return {
            name,
            type: sectionNameToType(el.parent().find('h2').first().text()) ?? typeFromPageTitle,
            html: $.html(processMemberHtml($, el, options)),
            kind: ITEM_KINDS.member
        }
    }).toArray();
    const membersWithOptions = members.map(member => ({
        ...member,
        className: options.className
    }));
    return membersWithOptions;
};

const genPathForMember = (memberDescriptor) =>
    path.join(outDirPath, memberDescriptor.className ?? 'Root', memberDescriptor.name + '.html')

const main = async () => {
    console.log('Simple Doc Gen');
    console.log('pwd', pwd().toString());
    cd(path.join(scriptPath, docPath));
    console.log('after cd', pwd().toString());
    const globalDirs = ['functions', 'interfaces', 'types', 'variables'];
    const dataFromModules = globalDirs
        .flatMap(dir => ls(dir).map(file => `${dir}/${file}`))
        .flatMap(file => processFile(file, {className: 'Root'}))
    ;
    const dataFromClasses = ls('classes')
        .map(clsFileName => processFile(`classes/${clsFileName}`, {className: path.basename(clsFileName, '.html')}));
    mkdir('-p', outDirPath);
    const dataCombined = [...dataFromModules, ...dataFromClasses.flat()];
    ShellString(JSON.stringify(dataCombined, null, 4)).to(path.join(outDirPath, 'data.json'));
    const {default: vento} = await import('ventojs');
    vto = vento({includes: scriptPath});
    const memberTemplate = await vto.load(path.join(scriptPath, './member.vto'));
    for (const member of dataCombined) {
        const filePath = genPathForMember(member);
        mkdir('-p', path.dirname(filePath));
        const memberResult = await memberTemplate(member);
        ShellString(memberResult.content).to(filePath);
    }
    const indexTemplate = await vto.load(path.join(scriptPath, './index.vto'));
    const membersByClassNameRaw = groupBy(x => x.className)(dataCombined);
    const membersByClassNameStructured = Object.entries(membersByClassNameRaw).map(([rawClsName, members]) => {
        const className = rawClsName === 'null' ? 'Root' : rawClsName;
        const sortKey = className === 'Root' ? ('_' + className) : (className === 'Opt' ? '__Opt' : className);
        return {
            className,
            members,
            sortKey
        };
    }).sort((a, b) => a.sortKey.localeCompare(b.sortKey));
    const indexResult = await indexTemplate({
        members: dataCombined,
        membersByClassName: membersByClassNameRaw,
        data: membersByClassNameStructured
    });
    ShellString(indexResult.content).to(path.join(outDirPath, 'index.html'));
    cat(path.join(scriptPath, 'styles.css')).to(path.join(outDirPath, 'styles.css'));
    console.log('Done');
};
main();
