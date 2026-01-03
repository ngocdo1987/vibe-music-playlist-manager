/**
 * Theme Toggle - Bootstrap 5 dark/light mode switcher
 * Saves preference to localStorage
 */

(function () {
    'use strict';

    const THEME_KEY = 'music-manager-theme';

    // Get saved theme or default to system preference
    function getPreferredTheme() {
        const savedTheme = localStorage.getItem(THEME_KEY);
        if (savedTheme) {
            return savedTheme;
        }
        return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
    }

    // Apply theme to document
    function setTheme(theme) {
        document.documentElement.setAttribute('data-bs-theme', theme);
        localStorage.setItem(THEME_KEY, theme);

        // Update any theme toggle icons
        const icons = document.querySelectorAll('.bi-moon-stars, .bi-sun');
        icons.forEach(icon => {
            if (theme === 'dark') {
                icon.classList.remove('bi-moon-stars');
                icon.classList.add('bi-sun');
            } else {
                icon.classList.remove('bi-sun');
                icon.classList.add('bi-moon-stars');
            }
        });
    }

    // Toggle between light and dark
    window.toggleTheme = function () {
        const currentTheme = document.documentElement.getAttribute('data-bs-theme');
        const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
        setTheme(newTheme);
    };

    // Apply theme on page load
    document.addEventListener('DOMContentLoaded', function () {
        setTheme(getPreferredTheme());
    });

    // Listen for system theme changes
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
        if (!localStorage.getItem(THEME_KEY)) {
            setTheme(e.matches ? 'dark' : 'light');
        }
    });
})();
