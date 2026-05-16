(function () {
    const list = document.getElementById("listing");
    if (!list) return;

    const buttons = document.querySelectorAll(".sortbtn");

    let sortKey = "name";
    let sortDir = 1;

    function norm(s) {
        return (s || "").toLowerCase();
    }

    function num(v) {
        const n = Number(v);
        return Number.isFinite(n) ? n : 0;
    }

    function updateIndicators() {
        buttons.forEach(btn => {
            const key = btn.dataset.sort;
            const base = key === "name" ? "Name" :
                key === "size" ? "Size" : "Modified";

            if (key === sortKey) {
                btn.textContent = base + (sortDir === 1 ? " ▲" : " ▼");
            } else {
                btn.textContent = base;
            }
        });
    }

    function compare(a, b) {
        const at = a.dataset.type || "file";
        const bt = b.dataset.type || "file";

        if (at !== bt) return at === "dir" ? -1 : 1;

        if (sortKey === "size") {
            return (num(a.dataset.size) - num(b.dataset.size)) * sortDir;
        }

        if (sortKey === "mtime") {
            return (num(a.dataset.mtime) - num(b.dataset.mtime)) * sortDir;
        }

        return norm(a.dataset.name).localeCompare(norm(b.dataset.name)) * sortDir;
    }

    function sortNow() {
        const items = Array.from(list.querySelectorAll(".entry"));
        items.sort(compare);

        const frag = document.createDocumentFragment();
        items.forEach(el => frag.appendChild(el));
        list.appendChild(frag);

        updateIndicators();
    }

    buttons.forEach(btn => {
        btn.addEventListener("click", () => {
            const key = btn.dataset.sort;

            if (sortKey === key) {
                sortDir *= -1;
            } else {
                sortKey = key;
                sortDir = 1;
            }

            sortNow();
        });
    });

    updateIndicators();
    sortNow();
})();