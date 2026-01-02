// Theme toggle functionality
(function () {
    'use strict';

    const THEME_KEY = 'music-manager-theme';

    // Get stored theme or default to light
    function getStoredTheme() {
        return localStorage.getItem(THEME_KEY) || 'light';
    }

    // Set theme on document
    function setTheme(theme) {
        document.documentElement.setAttribute('data-bs-theme', theme);
        localStorage.setItem(THEME_KEY, theme);
        updateToggleIcon(theme);
    }

    // Update toggle button icon
    function updateToggleIcon(theme) {
        const toggleBtn = document.getElementById('themeToggle');
        if (toggleBtn) {
            const icon = toggleBtn.querySelector('i');
            if (icon) {
                if (theme === 'dark') {
                    icon.className = 'bi bi-sun-fill';
                } else {
                    icon.className = 'bi bi-moon-fill';
                }
            }
        }
    }

    // Toggle between light and dark
    function toggleTheme() {
        const currentTheme = document.documentElement.getAttribute('data-bs-theme') || 'light';
        const newTheme = currentTheme === 'light' ? 'dark' : 'light';
        setTheme(newTheme);
    }

    // Initialize theme on page load
    function initTheme() {
        const theme = getStoredTheme();
        setTheme(theme);

        // Attach event listener to toggle button
        const toggleBtn = document.getElementById('themeToggle');
        if (toggleBtn) {
            toggleBtn.addEventListener('click', toggleTheme);
        }
    }

    // Run on DOM ready
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initTheme);
    } else {
        initTheme();
    }
})();
