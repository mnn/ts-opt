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

const processMember = (memberEl, options) => {
    return memberEl.attr('data-className', options.className);
}

const processFile = (inputFilePath, options = {className: null}) => {
    const html = cat(inputFilePath).toString();
    const $ = cheerio.load(html);

    const members = $('.tsd-member').map((i, rEl) => {
        const el = $(rEl);

        return {
            name: el.find('a.tsd-anchor').prop('id'),
            type: pipe(el.attr('class').split(/\s+/).find(x => x.startsWith(tsdKindPrefix)), dropStr(tsdKindPrefix.length)),
            html: $.html(processMember(el, options))
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
    const dataFromModules = processFile('modules.html');
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
    const indexResult = await indexTemplate({members: dataCombined, membersByClassName: membersByClassNameRaw, data: membersByClassNameStructured});
    ShellString(indexResult.content).to(path.join(outDirPath, 'index.html'));
};
main();
