(() => {
    const isEnglish = new URLSearchParams(window.location.search).get('lang') === 'en';
    const labels = {
        'Répartition des postes occupés par les adhérent.es CJSM': 'CJSM members by role',
        'Répartition des approches étudiées par les adhérent.es CJSM': 'Analytical approaches used by CJSM members',
        "Répartition des champs d'action des membres CJSM": 'Research fields of CJSM members',
        'Répartition des domaines étudiés par les adhérent.es CJSM': 'Application areas studied by CJSM members',
        'Doctorant': 'PhD student',
        'Ingénieur': 'Engineer',
        'Post-Doctorant': 'Postdoc',
        'Manager de projet': 'Project manager',
        'Praticien hospitalier en pharmacologie': 'Hospital pharmacology practitioner',
        'Développement Méthodologique': 'Method development',
        "Préparation d'échantillon": 'Sample preparation',
        'Informatique/Statistiques/Traitement de données': 'Computing/Statistics/Data processing',
        'Élucidation Structurale': 'Structural elucidation',
        'Imagerie': 'Imaging',
        'Mobilité ionique': 'Ion mobility',
        'Analyse de petites molécules par HRMS': 'Small-molecule analysis by HRMS',
        "Fondamental (étude d'énergie)": 'Fundamental research (energy studies)',
        'Protéomique': 'Proteomics',
        'Chimie Organique': 'Organic chemistry',
        'Métabolomique': 'Metabolomics',
        'Lipidomique': 'Lipidomics',
        'Polymères': 'Polymers',
        'Glycomique': 'Glycomics',
        'Pétroléomique': 'Petroleomics',
        'Chimie inorganique': 'Inorganic chemistry',
        'ARN': 'RNA',
        'Agents dopants': 'Doping agents',
        'Santé': 'Health',
        'Environnement': 'Environment',
        'Fondamental': 'Fundamental research',
        'Agro-alimentaire': 'Food and agriculture',
        'Criminalistique': 'Forensic science',
        'Patrimoine': 'Cultural heritage',
        'Chimie Industrielle': 'Industrial chemistry',
        'Régions françaises': 'French regions',
        'Pays frontaliers': 'Neighbouring countries',
        'Date de diffusion': 'Date posted',
        'Poste': 'Position',
        'Offre': 'Job opening',
        'Laboratoire': 'Laboratory',
        'Lieu': 'Location',
        'Date limite de réponse': 'Application deadline',
        "Lien vers l'offre": 'Job link',
        'Lien': 'Link',
        'Non diffusé': 'Not specified',
        'juillet 2026': 'July 2026',
    };

    window.translateEmbeddedLabel = text => {
        if (!isEnglish) return text;
        return labels[text] || text.replace(/ membres\b/g, ' members').replace(/>Lien</g, '>Link<');
    };

    if (!isEnglish) return;
    document.documentElement.lang = 'en';

    // Translate widget labels before HTMLWidgets reads its embedded JSON.
    // Leave coordinates, data values, field names and external URLs intact.
    document.querySelectorAll('script[type="application/json"]').forEach(script => {
        script.textContent = script.textContent.replace(/"(?:\\.|[^"\\])*"/g, token => {
            const original = JSON.parse(token);
            const translated = window.translateEmbeddedLabel(original);
            return original === translated ? token : JSON.stringify(translated);
        });
    });
})();
