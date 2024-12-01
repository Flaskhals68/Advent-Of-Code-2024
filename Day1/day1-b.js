const fs = require('fs').promises

async function parseFile(path) {
    try {
        const data = await fs.readFile(path, 'utf8');
        let lines = data.split('\n')
        let matrix = lines.map(line => line.trim().split(/\s+/));
        let transposed = matrix[0].map((_, colIndex) => matrix.map(row => parseInt(row[colIndex])));
        return transposed;
    } catch (err) {
        console.error(err);
        return [];
    }
}

function freqMap(arr) {
    let map = {};
    arr.forEach(element => {
        if (map[element] != null) {
            map[element]++;
        } else {
            map[element] = 1;
        }
    });
    return map;
} 

function similarityScore(a, b) {
    let map_b = freqMap(b);
    
    let score = a.reduce(
        (acc, val) => acc + (val * (map_b[val] || 0)), 0
    );
    return score;
}

async function main() {
    let lists = await parseFile('input.txt')
    let score = similarityScore(lists[0], lists[1])
    console.log(score)
}

main()