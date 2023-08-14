import fs from 'fs';
import path from 'path';
import { makeBadge } from 'badge-maker';
import { gzipSize } from 'gzip-size';
import UglifyJS from 'uglify-js';

const packageJsonPath = path.resolve('package.json');
const packageJsonContent = fs.readFileSync(packageJsonPath, 'utf-8');
const packageJson = JSON.parse(packageJsonContent);

const generateDependencyBadge = () => {
    const dependencyCount = Object.keys(packageJson.dependencies || {}).length;
    const dependencyBadgeFormat = {
        label: 'dependencies',
        message: dependencyCount.toString(),
        color: 'blue',
        labelColor: 'grey',
    };
    const dependencyBadgeSvg = makeBadge(dependencyBadgeFormat);
    fs.writeFileSync('badge-dependency.svg', dependencyBadgeSvg);
};

const calculateGzippedMinifiedContent = async (filePath) => {
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    const minifiedResult = UglifyJS.minify(fileContent, {
        compress: {
            passes: 3,
            sequences: true,
            dead_code: true,
            conditionals: true,
            booleans: true,
            unused: true,
            if_return: true,
            join_vars: true,
            drop_console: true,
        },
        mangle: {
            keep_fnames: true,
        },
        output: {
            comments: false,
        },
    });

    if (minifiedResult.error) {
        throw new Error(`Minification error in file ${file}: ${minifiedResult.error}`);
    }

    return minifiedResult.code;
};

const generatePackageSizeBadge = async () => {
    try {
        const distPath = 'dist';
        const jsFiles = fs.readdirSync(distPath).filter(file => file.endsWith('.js'));

        const filePaths = jsFiles.map(file => path.join(distPath, file));
        const minifiedContents = await Promise.all(filePaths.map(calculateGzippedMinifiedContent));
        const concatenatedMinifiedContent = minifiedContents.join('');

        const totalSize = await gzipSize(concatenatedMinifiedContent);
        const totalSizeKb = (totalSize / 1024).toFixed(1);
        const packageSizeBadgeFormat = {
            label: 'minzipped',
            message: `~ ${totalSizeKb} kB`,
            color: 'blue',
            labelColor: 'grey',
        };
        const packageSizeBadgeSvg = makeBadge(packageSizeBadgeFormat);
        fs.writeFileSync('badge-size.svg', packageSizeBadgeSvg);
    } catch (error) {
        console.error('Error generating package size badge:', error);
        process.exit(1);
    }
};

generateDependencyBadge();
generatePackageSizeBadge();
