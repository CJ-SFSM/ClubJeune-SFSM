(() => {
const sponsorRoot = document.getElementById("sponsoring-form");
    const checkboxes = sponsorRoot.querySelectorAll('.item');
    const totalSpan = sponsorRoot.querySelector('#total');
    const exportBox = sponsorRoot.querySelector('#exportBox');

    // Add event listeners to don libre inputs to update total on change
    sponsorRoot.querySelectorAll('.don-libre').forEach(input => {
        input.addEventListener('input', updateTotal);
    });

 /* Choix de la langue */
const frDiv = sponsorRoot.querySelector('#fr');
const enDiv = sponsorRoot.querySelector('#en');
const langBtn = sponsorRoot.querySelector('#langBtn');

langBtn.addEventListener('click', () => {
    // lire le style réel de l’élément
    const frVisible = window.getComputedStyle(frDiv).display !== "none";
    if (frVisible) {
        frDiv.style.display = "none";
        enDiv.style.display = "block";
        langBtn.textContent = "Français";  
    } else {        
        frDiv.style.display = "block";        
        enDiv.style.display = "none";
        langBtn.textContent = "English";
    }
    // Clear all checked checkboxes in both sections
    sponsorRoot.querySelectorAll('input[type="checkbox"]').forEach(cb => cb.checked = false);
    // Clear all don-libre values in both sections
    sponsorRoot.querySelectorAll('.don-libre').forEach(input => input.value = "");
    // Remise à zéro du total et du message
    updateTotal();
    // Remise à zéro de la zone d'export
    exportBox.value = "";
});

/* Fonction Calcul du montant total choisi*/
function updateTotal() {
    let total = 0;
    const frVisible = window.getComputedStyle(frDiv).display !== "none";
    const section = frVisible ? frDiv : enDiv;

    // cases cochées dans la langue active
    const activeCheckboxes = section.querySelectorAll('input[type="checkbox"]');
    activeCheckboxes.forEach(cb => { if (cb.checked) total += parseInt(cb.value); });

    // don libre actif
    const donLibreInput = section.querySelector('.don-libre');
    if (donLibreInput && donLibreInput.value) total += parseInt(donLibreInput.value);

    // Fix: update the correct total span for each language
    if (frVisible) {
        sponsorRoot.querySelector('#fr #total').textContent = total;
    } else {
        sponsorRoot.querySelector('#en #total').textContent = total;
    }

    // Message type de sponsoring //
    //const messageDiv = document.getElementById('message');
    if (frVisible) {
        const messageDiv = sponsorRoot.querySelector('#message_fr');
        if (total>0 && total< 400) {
            messageDiv.textContent = "Sponsor Basique";
            messageDiv.className = "basique";
        } else if (total>=400 && total < 600) {
            messageDiv.textContent = "Sponsor Classique";
            messageDiv.className = "classique";
        } else if (total>=600 && total < 1000) {
            messageDiv.textContent = "Sponsor Premium";
            messageDiv.className = "premium";
        } else if (total>=1000) {
            messageDiv.textContent = "Sponsor Prestige";
            messageDiv.className = "prestige";
        } else if (total===0) {
            messageDiv.textContent = "";
            messageDiv.className = "";
        }
    } else {
        const messageDiv = sponsorRoot.querySelector('#message_en');
        if (total>0 && total< 400) {
            messageDiv.textContent = "Basic Sponsoring";
            messageDiv.className = "basique";
        } else if (total>=400 && total < 600) {
            messageDiv.textContent = "Classic Sponsoring";
            messageDiv.className = "classique";
        } else if (total>=600 && total < 1000) {
            messageDiv.textContent = "Premium Sponsoring";
            messageDiv.className = "premium";
        } else if (total>=1000) {
            messageDiv.textContent = "Prestige Sponsoring";
            messageDiv.className = "prestige";
        } else if (total===0) {
            messageDiv.textContent = "";
            messageDiv.className = "";
        }
    }
}

/* Fonction Exporter la sélection */
function exportSelection() {
    // Tester la langue active (fr ou en)
    const frVisible = window.getComputedStyle(sponsorRoot.querySelector('#fr')).display !== "none";
    const isEnglish = !frVisible;
    
    // On choisit la bonne section (FR ou EN) en fonction du résultat du test
    const section = isEnglish ? sponsorRoot.querySelector('#en') : sponsorRoot.querySelector('#fr');
    const checkboxes = section.querySelectorAll('.item');
    const donLibreInput = section.querySelector('.don-libre');
    const exportBox = section.querySelector('#exportBox');
    
    let lines = [];
    let total = 0;

    // Options cochées
    checkboxes.forEach(cb => {
        if (cb.checked) {
        let price = parseInt(cb.value);
        let label = cb.getAttribute('data-label');
        total += price;
        lines.push("- " + label + (price > 0 ? " (" + price + " €)" : ""));
        }
    });

    // Don libre
    if (donLibreInput && donLibreInput.value && parseInt(donLibreInput.value) > 0) {
        let don = parseInt(donLibreInput.value);
        total += don;
        lines.push(isEnglish ? "- Custom donation (" + don + " €)" : "- Don libre (" + don + " €)");
    }

    // Total
    lines.push("\n" + (isEnglish ? "Total: " : "Total : ") + total + " €");
    exportBox.value = lines.join("\n");

    if (frVisible) {
        const messageDiv = sponsorRoot.querySelector('#message_fr');
        if (messageDiv.textContent) {
        lines.push("\n" + messageDiv.textContent);
        }
    } else {
        const messageDiv = sponsorRoot.querySelector('#message_en');
        if (messageDiv.textContent) {
        lines.push("\n" + messageDiv.textContent);
        }
    }
    
    exportBox.value = lines.join("\n");
}
    checkboxes.forEach(cb => cb.addEventListener('change', updateTotal));
   
  
sponsorRoot.querySelectorAll('[data-export-selection]').forEach(button => button.addEventListener('click', exportSelection));
})();
