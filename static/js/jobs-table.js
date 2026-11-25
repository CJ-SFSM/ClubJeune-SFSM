(() => {
    document.querySelectorAll('.jobs-table-frame').forEach(frame => {
        let observer;

        const fitTable = () => {
            if (observer) observer.disconnect();

            const doc = frame.contentDocument;
            const container = doc?.getElementById('htmlwidget_container');
            if (!container) return;

            doc.body.style.margin = '0';
            doc.body.style.padding = '0';
            doc.documentElement.lang = document.documentElement.lang;

            const resize = () => {
                const height = `${Math.ceil(container.getBoundingClientRect().height) + 1}px`;
                if (frame.style.height !== height) frame.style.height = height;
            };

            observer = new ResizeObserver(resize);
            observer.observe(container);
            resize();
        };

        frame.addEventListener('load', fitTable);
        if (frame.contentDocument?.readyState === 'complete') fitTable();
    });
})();
