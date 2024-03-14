const gridmaker = {
    gridElem: document.querySelector('grid'),
    ioElem: document.querySelector('io'),
    buttonsElem: document.querySelector('buttons'),

    colors: {
        wall: "rgb(100, 0, 100)",
        empty: "rgb(37, 34, 37)",
        start: "rgb(0, 100, 0)",
        end: "rgb(100, 0, 0)",
    },

    buildState: null,

    ios: [
        {text: "Reset ❌", callback: () => {gridmaker.createGridSquares()}},
        {text: "Save ✅", callback: () => {gridmaker.createTxt(gridmaker.saveGrid())}},
    ],

    buttons: [
        {text: "Wall 🧱", state: "wall"},
        {text: "Empty 🔍", state: "empty"},
        {text: "Start 🟢", state: "start"},
        {text: "End 🏁", state: "end"},
    ],

    createButtons: () => {
        gridmaker.buttonsElem.innerHTML = ''
        gridmaker.ioElem.innerHTML = ''

        gridmaker.buttons.forEach(b => {
            const button = document.createElement('button')
            button.innerText = b.text 
            button.onclick = () => {
                gridmaker.setActive(b.text)
                gridmaker.buildState = b.state
            }
            gridmaker.buttonsElem.append(button)
        })
        gridmaker.ios.forEach(i => {
            const button = document.createElement('button')
            button.innerText = i.text
            button.onclick = i.callback
            gridmaker.ioElem.append(button)
        })
    },

    setActive: (bName) => {
        const buttons = Array.from(gridmaker.buttonsElem.children)
        buttons.forEach(button => {
            if (button.innerText == bName) button.classList.add('active')
            else button.classList.remove('active')
        })
    },

    createGridSquares: () => {
        gridmaker.gridElem.innerHTML = ''
        for (let y = 0; y < 16; y++) {
            for (let x = 0; x < 16; x++) {
                const gridSquare = document.createElement('grid-square')

                if (x == 0 || x == 15 || y == 0 || y == 15) {
                    gridSquare.style.backgroundColor = gridmaker.colors.wall
                    gridSquare.style.cursor = 'not-allowed'
                } else {
                    gridSquare.classList.add('canHoverSquare')
                    gridSquare.onclick = () => {
                        if (gridmaker.buildState) gridSquare.style.backgroundColor = gridmaker.colors[gridmaker.buildState]
                    }
                }

                gridmaker.gridElem.append(gridSquare)
            }
        }
    },

    saveGrid: () => {
        const squares = Array.from(gridmaker.gridElem.children)

        let mapString = ''
        let colour = null

        squares.forEach((s, idx) => {
            
            if (idx % 16 == 0 && idx !== 0) mapString += '\n'
            
            colour = s.style.backgroundColor
            if (colour == gridmaker.colors.wall) mapString += '#'
            else if (colour == gridmaker.colors.start) mapString += 'S'
            else if (colour == gridmaker.colors.end) mapString += 'G'
            else mapString += ' '
        })
        return mapString
    },

    createTxt: (content) => {
        const blob = new Blob([content], {type: 'text/plain'})
        const link = document.createElement('a')
        link.download = 'newMaze.txt'
        link.href = URL.createObjectURL(blob)
        document.body.appendChild(link)
        link.click()
        document.body.removeChild(link)
    },

    __init__: () => {
        gridmaker.createButtons()
        gridmaker.createGridSquares()
    }
}
gridmaker.__init__()