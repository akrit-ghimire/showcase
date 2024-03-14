const navigation = {
    tabContainer: document.querySelector('tabs'),
    currentPage: null,
    
    pages: {
        "create": {
            page: document.querySelector('create-page'),
            text: "Create Maze",
            button: null,
            callback: () => {
                canvasControl.stopWatching()
            }
        },
        "view": {
            page: document.querySelector('body-cam-page'),
            text: "Watch BodyCam",
            button: null,
            callback: () => {
                canvasControl.startWatching()
            }
        }
    },

    createTabButtons: () => {
        navigation.tabContainer.innerHTML = ''

        Object.keys(navigation.pages).forEach(pName => {
            const page = navigation.pages[pName]
            const buttonElem = document.createElement('button')

            buttonElem.innerText = page.text
            
            buttonElem.onclick = () => {
                navigation.changePageTo(pName)
            }

            page.button = buttonElem

            navigation.tabContainer.append(buttonElem)
        })
    },

    changePageTo: (desiredPageName) => {
        Object.keys(navigation.pages).forEach(pName => {
            const page = navigation.pages[pName]
            
            if (pName == desiredPageName) {
                page.page.style.display = 'flex'
                if (page.button) page.button.classList.add('active')
                if (page.callback) page.callback()
            }
            else {
                page.page.style.display = 'none'
                if (page.button) page.button.classList.remove('active')
            } 
        })
    },

    __init__: () => {
        navigation.createTabButtons()
        navigation.currentPage = "create"
        navigation.changePageTo(navigation.currentPage)
    }
}
navigation.__init__()