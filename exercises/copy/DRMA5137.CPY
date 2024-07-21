      **************************************************************
      *
      *     MITCOPY: BEZEICHNUNG DER FIBU-KONTEN
      *
      *     SOURCE-NAME      :  DRMA5137
      *
      *     GENERIERUNG      :  KEINE
      *     DURCH            :
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00217!21.03.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00263!12.05.16!2.2  ! NEUE UST-KONTEN FÜR       ! WOLFF
      *         !        !     ! SONDERLEISTUNGEN          !
      * AMA00263!15.07.16!2.3  ! ÄNDERUNG KONTO FÜR BANK-  ! D.WILHELM
      *         !        !     ! SAMMELÜBERWEISUNG VON     !
      *         !        !     ! 257000 NACH 282700        !
      * AMA00303!20.12.16!2.4  ! BKK-LINDE ALS EIGEN-      ! OBER
      *         !        !     ! STAENDIGE GESELLSCHAFT    !
      * AMA00342!23.02.17!2.5  ! TELESON NEUE UST-PFL.GES. ! A.Strauss
      *         !        !2.6  ! Korrektur occurs Allfinanz! A.Strauss
      *         !        !2.7  ! BKK-LINDE jetzt BKK Linde ! A.Strauss
      *         !        !2.8  ! Zusatzbonus ausgebaut     ! O.Wilhelm
      *         !        !     ! (kommt aus AMA00302)      !
      * AMA00350!08.05.17!2.9  ! Neue PG 93, 96 u. 98      ! S.Warz
      *         !        !bis  !                           !
      *         !        !2.13 ! Korrektur der Reihenfolge ! HSE
      * --------------------------------------------------------------
      * AMA00356!01.09.17!2.14 ! Neue PG 87 (Cadooz)       ! HSE
      *         !        !     !         33 (Capital Bank) !
      * --------!--------!-----!---------------------------!------------
      * AMA00367!20.11.17!2.15 ! Neuer PG 79 (Geiger EM)   ! O.Wilhelm
      * --------!--------!-----!---------------------------!------------
      * AMA00400!26.02.18!2.16 ! Erweiterung für           ! Warz
      *         !        !     ! Allfinanz AG Ges 11,21,42 !
      * --------!--------!-----!---------------------------!------------
      * AMA00417!09.11.18!2.17 ! Neuer PG 12 (GC&C)        ! Thurmann
      *         !        !2.18 ! Fehlerkorrektur für Wien  !
      *         !        !2.19 ! 182 auf 186               !
      *         !        !2.20 ! Syntaxkorrekturen         !
      * --------!--------!-----!---------------------------!------------
      * AMA00434!11.10.19!2.21 ! One Company: Änderung PG  ! O.Wilhelm
      *         !        !     ! 10, 11 und 21; Entfernen  !
      *         !        !     ! historischer FiBu-Konten  !
      * --------!--------!-----!---------------------------!------------
      * AMA00433!17.10.19!2.22 ! Neuer PG 92               ! A.Strauss
      *         !        !     ! 'Planeth Im' (PlanetHome) !
      *         !        !     ! 'So. Ges 2' alt gelöscht  !
      *         !        !     ! +4 elem -5 elem: 163-> 162!
      * --------!--------!-----!---------------------------!------------
      * AMA00447!07.11.19!2.23 ! Neuer PG 61 DVAG-Hyp      ! S.Warz
      *         !        !     ! 'DVAG-Hyp'                !
      *         !        !     ! +1 elem          162-> 165!
      * --------!--------!-----!---------------------------!------------
      * AMA00473!01.04.20!2.24 ! Neuer PG 86 'JURA DIREKT' ! A.Strauss
      *         !        !2.25 ! +4 elem          165-> 169!
      * AMA00458!        !     ! 63812xExistenzgründerbonus!
      *         !        !     ! ausgebaut 169-3elem => 166!
      * --------!--------!-----!---------------------------!------------
      * AMA00480!16.06.20!2.26 ! One Company: Änderung PG  ! D.Wilhelm
      *         !        !2.27 ! 20, 50 und 63             !
      * --------!--------!-----!---------------------------!----------
      * AMA00481!23.06.20!2.28 ! Anpassung der Umsatz-     ! O.Wilhelm
      *         !        !     ! steuersätze               !
220822* --------!--------!-----!---------------------------!----------
220822* MA-1545 !22.08.22!--   ! Neuer PG 80 'Geiger USt'  ! O.Wilhelm  
220822* --------!--------!-----!---------------------------!----------
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *---------------------------------------------------------------*
      * Kurzbeschreibung:
      * ================
      *    Diese Copy-Strecke ist die Zuordnung der Bezeichnung
      *    zu den FiBu-Konten je Vertriebsgesellschaft.
      *
      *
      * Voraussetzungen:
      * ===============
      *    keine
      *
      *
      * Funktionale Beschreibung:
      * ========================
      *    Zu jeder Vertriebsorganisation ist festgehalten, wie
      *    die Bezeichnung zu den FiBu-Konten heißt.
      *
      *    Damit die Zugriffe ordnungsgemäß und performant
      *    funktionieren, ist auf eine korrekte Angabe in
      *    TABBEZKTO-VG-nnnn-MAX zu achten. Ebenso müssen Konten
      *    aufsteigend sortiert sein.
      *
      *    Die Abkürzung des Tabellennamens leitet sich wie folgt ab:
      *       BEZKTO: Bezeichnung FiBu-Konto
      *               ---              -  --
      *
      *
      * Schnittstellen:
      * ==============
      *    Keine besonderen Erläuterungen notwendig.
      *
      *---------------------------------------------------------------*
       01  TABBEZKTO-FELDER.
220822*    05 TABBEZKTO-VG-MAX         pic S9(4) binary sync value 186.
220822*    05 TABBEZKTO-VG-1301-MAX    pic S9(4) binary sync value 184.
220822*    05 TABBEZKTO-VG-1302-MAX    pic S9(4) binary sync value 14.
220822*    05 TABBEZKTO-VG-1303-MAX    pic S9(4) binary sync value 175.
220822*    05 TABBEZKTO-VG-0000-MAX    pic S9(4) binary sync value 001.
220822*    05 TABBEZKTO-VG-1305-MAX    pic S9(4) binary sync value 186.
220822     05 TABBEZKTO-VG-MAX         pic S9(4) binary sync value 193.
220822     05 TABBEZKTO-VG-1301-MAX    pic S9(4) binary sync value 191.
220822     05 TABBEZKTO-VG-1302-MAX    pic S9(4) binary sync value 14.
220822     05 TABBEZKTO-VG-1303-MAX    pic S9(4) binary sync value 182.
220822     05 TABBEZKTO-VG-0000-MAX    pic S9(4) binary sync value 001.
220822     05 TABBEZKTO-VG-1305-MAX    pic S9(4) binary sync value 193.
      *
       01  TABELLE-BEZKTO.
           05 TABBEZKTO-WERTE.
      *
      * Beginn DVAG
      *
            07 TABBEZKTO-WERTE-DVAG.
              10 pic X(36) value '150100Forderungen GEDLeben          '.
              10 pic X(36) value '150200Forderungen GEDVers           '.
              10 pic X(36) value '150300Forderungen GEDKranken        '.
              10 pic X(36) value '150400Forderungen ADV-RS            '.
              10 pic X(36) value '150500Forderungen DMB-RS            '.
              10 pic X(36) value '150925Forderungen GC&C              '.
              10 pic X(36) value '151100Forderungen AGI               '.
              10 pic X(36) value '151700Forderungen DWS               '.
              10 pic X(36) value '152000Forderungen DB                '.
              10 pic X(36) value '152100Forderungen Santander         '.
              10 pic X(36) value '152200Forderungen BADENIA           '.
              10 pic X(36) value '152300Forderungen GEDHyp.-Da        '.
              10 pic X(36) value '152800Forderungen CB                '.
              10 pic X(36) value '152900Forderungen HYPO              '.
              10 pic X(36) value '153000Forderungen DSL Bank          '.
              10 pic X(36) value '155100Forderungen BKK Linde         '.
              10 pic X(36) value '156100Forderungen DVAG-Hyp          '.
              10 pic X(36) value '157900Forderungen Geiger EM         '.
220822        10 pic X(36) value '158091Forderungen Geiger USt        '.
              10 pic X(36) value '158691Forderungen JURADIREKT        '.
              10 pic X(36) value '158791Forderungen Cadooz            '.
              10 pic X(36) value '159000Forderungen SO.-GES           '.
              10 pic X(36) value '159091Forderungen DV                '.
              10 pic X(36) value '159291Forderungen Planeth Im        '.
              10 pic X(36) value '159391Forderungen Devolo            '.
              10 pic X(36) value '159491Forderungen TeleSon           '.
              10 pic X(36) value '159691Forderungen FingerHaus        '.
              10 pic X(36) value '159891Forderungen Lohnwerk          '.
              10 pic X(36) value '173100Forderungen an DVAG Wien      '.
              10 pic X(36) value '282700Bank Sammelüberweisung        '.
              10 pic X(36) value '443100Verbindlichkeiten an DVAG Wien'.
              10 pic X(36) value '460000Vorsteuer-Konto 19%           '.
              10 pic X(36) value '460310Umsatzsteuer-Konto 19%        '.
              10 pic X(36) value '460412Umsatzsteuer-Konto 16%        '.
              10 pic X(36) value '500001Prov.Einnahmen JURADIREKT/AP  '.
              10 pic X(36) value '500002Prov.Einnahmen DV/AP          '.
              10 pic X(36) value '500003Prov.Einnahmen DV/FP          '.
              10 pic X(36) value '500004Prov.Einnahmen Planeth Im/AP  '.
              10 pic X(36) value '500005Prov.Einnahmen Devolo/FP      '.
              10 pic X(36) value '500006Prov.Einnahmen TeleSon/AP     '.
              10 pic X(36) value '500007Prov.Einnahmen TeleSon/FP     '.
              10 pic X(36) value '500008Prov.Einnahmen FingerHaus/AP  '.
              10 pic X(36) value '500009Prov.Einnahmen Cadooz/AP      '.
              10 pic X(36) value '500010Prov.Einnahmen Cadooz/FP      '.
              10 pic X(36) value '500011Prov.Einnahmen Lohnwerk/AP    '.
              10 pic X(36) value '500012Prov.Einnahmen Lohnwerk/FP    '.
              10 pic X(36) value '500100Prov.Einnahmen GEDLeben/AP    '.
              10 pic X(36) value '500105Prov.Einnahmen GEDLeben/FP    '.
              10 pic X(36) value '500200Prov.Einnahmen GEDVers/AP     '.
              10 pic X(36) value '500205Prov.Einnahmen GEDVers/FP     '.
              10 pic X(36) value '500300Prov.Einnahmen GEDKranken/AP  '.
              10 pic X(36) value '500305Prov.Einnahmen GEDKranken/FP  '.
              10 pic X(36) value '500400Prov.Einnahmen ADV-RS/AP      '.
              10 pic X(36) value '500405Prov.Einnahmen ADV-RS/FP      '.
              10 pic X(36) value '500500Prov.Einnahmen DMB-RS/AP      '.
              10 pic X(36) value '500505Prov.Einnahmen DMB-RS/FP      '.
              10 pic X(36) value '501100Prov.Einnahmen AGI/AP         '.
              10 pic X(36) value '501105Prov.Einnahmen AGI/FP         '.
              10 pic X(36) value '501700Prov.Einnahmen DWS/AP         '.
              10 pic X(36) value '501705Prov.Einnahmen DWS/FP         '.
              10 pic X(36) value '502000Prov.Einnahmen DB/AP          '.
              10 pic X(36) value '502005Prov.Einnahmen DB/FP          '.
              10 pic X(36) value '502100Prov.Einnahmen Santander/AP   '.
              10 pic X(36) value '502105Prov.Einnahmen Santander/FP   '.
              10 pic X(36) value '502200Prov.Einnahmen BADENIA/AP     '.
              10 pic X(36) value '502205Prov.Einnahmen BADENIA/FP     '.
              10 pic X(36) value '502300Prov.Einnahmen GEDHyp.-Da/AP  '.
              10 pic X(36) value '502305Prov.Einnahmen GEDHyp.-Da/FP  '.
              10 pic X(36) value '502800Prov.Einnahmen CB/AP          '.
              10 pic X(36) value '502805Prov.Einnahmen CB/FP          '.
              10 pic X(36) value '502900Prov.Einnahmen HYPO/AP        '.
              10 pic X(36) value '502905Prov.Einnahmen HYPO/FP        '.
              10 pic X(36) value '503000Prov.Einnahmen DSL Bank/AP    '.
              10 pic X(36) value '503005Prov.Einnahmen DSL Bank/FP    '.
              10 pic X(36) value '505100Prov.Einnahmen BKK Linde/AP   '.
              10 pic X(36) value '505105Prov.Einnahmen BKK Linde/FP   '.
              10 pic X(36) value '506100Prov.Einnahmen DVAG-Hyp/AP    '.
              10 pic X(36) value '507000Prov.Einnahmen SO.-GES/AP     '.
              10 pic X(36) value '507001Prov.Einnahmen Geiger EM/AP   '.
              10 pic X(36) value '507002Prov.Einnahmen GC&C/AP        '.
              10 pic X(36) value '507005Prov.Einnahmen SO.-GES/FP     '.
              10 pic X(36) value '507006Prov.Einnahmen Geiger EM/FP   '.
              10 pic X(36) value '507007Prov.Einnahmen GC&C/FP        '.
220822        10 pic X(36) value '508072Prov.Einnahmen Geiger USt/AP  '.
220822        10 pic X(36) value '508073Prov.Einnahmen Geiger USt/FP  '.
              10 pic X(36) value '508672Prov.Einnahmen JURADIREKT/AP  '.
              10 pic X(36) value '509092Prov.Einnahmen DV/AP          '.
              10 pic X(36) value '509093Prov.Einnahmen DV/FP          '.
              10 pic X(36) value '509272Prov.Einnahmen Planeth Im/AP  '.
              10 pic X(36) value '509393Prov.Einnahmen Devolo/FP      '.
              10 pic X(36) value '509492Prov.Einnahmen TeleSon/AP     '.
              10 pic X(36) value '509493Prov.Einnahmen TeleSon/FP     '.
              10 pic X(36) value '509692Prov.Einnahmen FingerHaus/AP  '.
              10 pic X(36) value '509872Prov.Einnahmen Cadooz/AP      '.
              10 pic X(36) value '509873Prov.Einnahmen Cadooz/FP      '.
              10 pic X(36) value '509892Prov.Einnahmen Lohnwerk/AP    '.
              10 pic X(36) value '509893Prov.Einnahmen Lohnwerk/FP    '.
              10 pic X(36) value '630001Prov.Ausgaben  JURADIREKT/AP U'.
              10 pic X(36) value '630002Prov.Ausgaben  DV/AP U        '.
              10 pic X(36) value '630003Prov.Ausgaben  DV/FP U        '.
              10 pic X(36) value '630004Prov.Ausgaben  Planeth Im/AP U'.
              10 pic X(36) value '630005Prov.Ausgaben  Devolo/FP U    '.
              10 pic X(36) value '630006Prov.Ausgaben  TeleSon/AP U   '.
              10 pic X(36) value '630007Prov.Ausgaben  TeleSon/FP U   '.
              10 pic X(36) value '630008Prov.Ausgaben  FingerHaus/AP U'.
              10 pic X(36) value '630009Prov.Ausgaben  Cadooz/AP U    '.
              10 pic X(36) value '630010Prov.Ausgaben  Cadooz/FP U    '.
              10 pic X(36) value '630011Prov.Ausgaben  Lohnwerk/AP U  '.
              10 pic X(36) value '630012Prov.Ausgaben  Lohnwerk/FP U  '.
              10 pic X(36) value '630100Prov.Ausgaben  GEDLeben/AP    '.
              10 pic X(36) value '630105Prov.Ausgaben  GEDLeben/FP    '.
              10 pic X(36) value '630200Prov.Ausgaben  GEDVers/AP     '.
              10 pic X(36) value '630205Prov.Ausgaben  GEDVers/FP     '.
              10 pic X(36) value '630300Prov.Ausgaben  GEDKranken/AP  '.
              10 pic X(36) value '630305Prov.Ausgaben  GEDKranken/FP  '.
              10 pic X(36) value '630400Prov.Ausgaben  ADV-RS/AP      '.
              10 pic X(36) value '630405Prov.Ausgaben  ADV-RS/FP      '.
              10 pic X(36) value '630500Prov.Ausgaben  DMB-RS/AP      '.
              10 pic X(36) value '630505Prov.Ausgaben  DMB-RS/FP      '.
              10 pic X(36) value '631100Prov.Ausgaben  AGI/AP         '.
              10 pic X(36) value '631105Prov.Ausgaben  AGI/FP         '.
              10 pic X(36) value '631700Prov.Ausgaben  DWS/AP         '.
              10 pic X(36) value '631705Prov.Ausgaben  DWS/FP         '.
              10 pic X(36) value '632000Prov.Ausgaben  DB/AP          '.
              10 pic X(36) value '632005Prov.Ausgaben  DB/FP          '.
              10 pic X(36) value '632100Prov.Ausgaben  Santander/AP   '.
              10 pic X(36) value '632105Prov.Ausgaben  Santander/FP   '.
              10 pic X(36) value '632200Prov.Ausgaben  BADENIA/AP     '.
              10 pic X(36) value '632205Prov.Ausgaben  BADENIA/FP     '.
              10 pic X(36) value '632300Prov.Ausgaben  GEDHyp.-Da/AP  '.
              10 pic X(36) value '632305Prov.Ausgaben  GEDHyp.-Da/FP  '.
              10 pic X(36) value '632800Prov.Ausgaben  CB/AP          '.
              10 pic X(36) value '632805Prov.Ausgaben  CB/FP          '.
              10 pic X(36) value '632900Prov.Ausgaben  HYPO/AP        '.
              10 pic X(36) value '632905Prov.Ausgaben  HYPO/FP        '.
              10 pic X(36) value '633000Prov.Ausgaben  DSL Bank/AP    '.
              10 pic X(36) value '633005Prov.Ausgaben  DSL Bank/FP    '.
              10 pic X(36) value '635100Prov.Ausgaben  BKK Linde/AP   '.
              10 pic X(36) value '635105Prov.Ausgaben  BKK Linde/FP   '.
              10 pic X(36) value '636100Prov.Ausgaben  DVAG-Hyp/AP    '.
              10 pic X(36) value '637100Prov.Ausgaben  SO.-GES/AP     '.
              10 pic X(36) value '637101Prov.Ausgaben  Geiger EM/AP   '.
              10 pic X(36) value '637102Prov.Ausgaben  GC&C/AP        '.
              10 pic X(36) value '637105Prov.Ausgaben  SO.-GES/FP     '.
              10 pic X(36) value '637106Prov.Ausgaben  Geiger EM/FP   '.
              10 pic X(36) value '637107Prov.Ausgaben  GC&C/FP        '.
220822        10 pic X(36) value '638072Prov.Ausgaben  Geiger USt/AP K'.
220822        10 pic X(36) value '638073Prov.Ausgaben  Geiger USt/FP K'.
220822        10 pic X(36) value '638074Prov.Ausgaben  Geiger USt/AP U'.
220822        10 pic X(36) value '638075Prov.Ausgaben  Geiger USt/FP U'.
              10 pic X(36) value '638100Erfolgsprovision              '.
              10 pic X(36) value '638108Erfolgsprovision KU           '.
              10 pic X(36) value '638109Erfolgsprovision U            '.
              10 pic X(36) value '638160Existenzgründer-Bonus (Neu)   '.
              10 pic X(36) value '638168Existenzgründer-Bonus (Neu) KU'.
              10 pic X(36) value '638169Existenzgründer-Bonus (Neu) U '.
              10 pic X(36) value '638672Prov.Ausgaben  JURADIREKT/AP K'.
              10 pic X(36) value '638674Prov.Ausgaben  JURADIREKT/AP U'.
              10 pic X(36) value '639092Prov.Ausgaben  DV/AP KU       '.
              10 pic X(36) value '639093Prov.Ausgaben  DV/FP KU       '.
              10 pic X(36) value '639094Prov.Ausgaben  DV/AP U        '.
              10 pic X(36) value '639095Prov.Ausgaben  DV/FP U        '.
              10 pic X(36) value '639272Prov.Ausgaben  Planeth Im/AP K'.
              10 pic X(36) value '639274Prov.Ausgaben  Planeth Im/AP U'.
              10 pic X(36) value '639393Prov.Ausgaben  Devolo/FP KU   '.
              10 pic X(36) value '639395Prov.Ausgaben  Devolo/FP U    '.
              10 pic X(36) value '639492Prov.Ausgaben  TeleSon/AP KU  '.
              10 pic X(36) value '639493Prov.Ausgaben  TeleSon/FP KU  '.
              10 pic X(36) value '639494Prov.Ausgaben  TeleSon/AP U   '.
              10 pic X(36) value '639495Prov.Ausgaben  TeleSon/FP U   '.
              10 pic X(36) value '639692Prov.Ausgaben  FingerHaus/AP K'.
              10 pic X(36) value '639694Prov.Ausgaben  FingerHaus/AP U'.
              10 pic X(36) value '639872Prov.Ausgaben  Cadooz/AP KU   '.
              10 pic X(36) value '639873Prov.Ausgaben  Cadooz/FP KU   '.
              10 pic X(36) value '639874Prov.Ausgaben  Cadooz/AP U    '.
              10 pic X(36) value '639875Prov.Ausgaben  Cadooz/FP U    '.
              10 pic X(36) value '639892Prov.Ausgaben  Lohnwerk/AP KU '.
              10 pic X(36) value '639893Prov.Ausgaben  Lohnwerk/FP KU '.
              10 pic X(36) value '639894Prov.Ausgaben  Lohnwerk/AP U  '.
              10 pic X(36) value '639895Prov.Ausgaben  Lohnwerk/FP U  '.
              10 pic X(36) value '909000EDV-Zwischenkonto Aachen      '.
              10 pic X(36) value '909100EDV-Zwischenkonto Frankfurt   '.
              10 pic X(36) value '909600EDV-Zwischenkonto CPD         '.
              10 pic X(36) value '909700EDV-Zwischenkonto Bestand     '.
              10 pic X(36) value '912000Ausgleich Vertriebsfonds      '.
              10 pic X(36) value '913000Ausgleich Verwaltungsfonds    '.
              10 pic X(36) value '913500Ausgleich USt.-Fonds          '.
              10 pic X(36) value '914000Ausgleich DVAG Wien-Mitarb.   '.
              10 pic X(36) value '919000Ausgleich EDV-Konten DVAG     '.
              10 pic X(36) value '919200Ausgleich Saldovorträge       '.
              10 pic X(36) value '930000Ausgleich DVAG Wien-Gesellsch.'.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
      * Beginn DVAG-Wien
      *
            07 TABBEZKTO-WERTE-WIEN.
              10 pic X(36) value '171000Forderungen an DVAG           '.
              10 pic X(36) value '246000Bank Sammelüberweisung        '.
              10 pic X(36) value '441000Verbindlichkeiten an DVAG     '.
              10 pic X(36) value '638100Erfolgsprovision              '.
              10 pic X(36) value '638160Existenzgründer-Bonus (Neu)   '.
              10 pic X(36) value '909000EDV-Zwischenkonto Aachen      '.
              10 pic X(36) value '909100EDV-Zwischenkonto Wien        '.
              10 pic X(36) value '909800EDV-Zwischenkonto Bestand     '.
              10 pic X(36) value '912000Ausgleich Vertriebsfonds      '.
              10 pic X(36) value '913000Ausgleich Verwaltungsfonds    '.
              10 pic X(36) value '914000Ausgleich DVAG-Mitarbeiter    '.
              10 pic X(36) value '919000Ausgleich EDV-Konten DVAG Wien'.
              10 pic X(36) value '919200Ausgleich Saldovorträge       '.
              10 pic X(36) value '930000Ausgleich DVAG-Gesellschaften '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
      *
      * Beginn Allfinanz
      *
            07 TABBEZKTO-WERTE-ALLF.
              10 pic X(36) value '150100Forderungen GEDLeben          '.
              10 pic X(36) value '150200Forderungen GEDVers           '.
              10 pic X(36) value '150300Forderungen GEDKranken        '.
              10 pic X(36) value '150400Forderungen ADV-RS            '.
              10 pic X(36) value '150925Forderungen GC&C              '.
              10 pic X(36) value '151100Forderungen AGI               '.
              10 pic X(36) value '151700Forderungen DWS               '.
              10 pic X(36) value '152000Forderungen DB                '.
              10 pic X(36) value '152100Forderungen Santander         '.
              10 pic X(36) value '152200Forderungen BADENIA           '.
              10 pic X(36) value '152300Forderungen GEDHyp.-Da        '.
              10 pic X(36) value '152800Forderungen CB                '.
              10 pic X(36) value '152900Forderungen HYPO              '.
              10 pic X(36) value '153000Forderungen DSL Bank          '.
              10 pic X(36) value '155100Forderungen BKK Linde         '.
              10 pic X(36) value '156100Forderungen DVAG-Hyp          '.
              10 pic X(36) value '157900Forderungen Geiger EM         '.
220822        10 pic X(36) value '158091Forderungen Geiger USt        '.
              10 pic X(36) value '158691Forderungen JURADIREKT        '.
              10 pic X(36) value '158791Forderungen Cadooz            '.
              10 pic X(36) value '159000Forderungen SO.-GES           '.
              10 pic X(36) value '159091Forderungen DV                '.
              10 pic X(36) value '159291Forderungen Planeth Im        '.
              10 pic X(36) value '159391Forderungen Devolo            '.
              10 pic X(36) value '159491Forderungen TeleSon           '.
              10 pic X(36) value '159691Forderungen FingerHaus        '.
              10 pic X(36) value '159891Forderungen Lohnwerk          '.
              10 pic X(36) value '282700Bank Sammelüberweisung        '.
              10 pic X(36) value '460000Vorsteuer-Konto 19%           '.
              10 pic X(36) value '460310Umsatzsteuer-Konto 19%        '.
              10 pic X(36) value '460412Umsatzsteuer-Konto 16%        '.
              10 pic X(36) value '500001Prov.Einnahmen JURADIREKT/AP  '.
              10 pic X(36) value '500002Prov.Einnahmen DV/AP          '.
              10 pic X(36) value '500003Prov.Einnahmen DV/FP          '.
              10 pic X(36) value '500004Prov.Einnahmen Planeth Im/AP  '.
              10 pic X(36) value '500005Prov.Einnahmen Devolo/FP      '.
              10 pic X(36) value '500006Prov.Einnahmen TeleSon/AP     '.
              10 pic X(36) value '500007Prov.Einnahmen TeleSon/FP     '.
              10 pic X(36) value '500008Prov.Einnahmen FingerHaus/AP  '.
              10 pic X(36) value '500009Prov.Einnahmen Cadooz/AP      '.
              10 pic X(36) value '500010Prov.Einnahmen Cadooz/FP      '.
              10 pic X(36) value '500011Prov.Einnahmen Lohnwerk/AP    '.
              10 pic X(36) value '500012Prov.Einnahmen Lohnwerk/FP    '.
              10 pic X(36) value '500100Prov.Einnahmen GEDLeben/AP    '.
              10 pic X(36) value '500105Prov.Einnahmen GEDLeben/FP    '.
              10 pic X(36) value '500200Prov.Einnahmen GEDVers/AP     '.
              10 pic X(36) value '500205Prov.Einnahmen GEDVers/FP     '.
              10 pic X(36) value '500300Prov.Einnahmen GEDKranken/AP  '.
              10 pic X(36) value '500305Prov.Einnahmen GEDKranken/FP  '.
              10 pic X(36) value '500400Prov.Einnahmen ADV-RS/AP      '.
              10 pic X(36) value '500405Prov.Einnahmen ADV-RS/FP      '.
              10 pic X(36) value '501100Prov.Einnahmen AGI/AP         '.
              10 pic X(36) value '501105Prov.Einnahmen AGI/FP         '.
              10 pic X(36) value '501700Prov.Einnahmen DWS/AP         '.
              10 pic X(36) value '501705Prov.Einnahmen DWS/FP         '.
              10 pic X(36) value '502000Prov.Einnahmen DB/AP          '.
              10 pic X(36) value '502005Prov.Einnahmen DB/FP          '.
              10 pic X(36) value '502100Prov.Einnahmen Santander/AP   '.
              10 pic X(36) value '502105Prov.Einnahmen Santander/FP   '.
              10 pic X(36) value '502200Prov.Einnahmen BADENIA/AP     '.
              10 pic X(36) value '502205Prov.Einnahmen BADENIA/FP     '.
              10 pic X(36) value '502300Prov.Einnahmen GEDHyp.-Da/AP  '.
              10 pic X(36) value '502305Prov.Einnahmen GEDHyp.-Da/FP  '.
              10 pic X(36) value '502800Prov.Einnahmen CB/AP          '.
              10 pic X(36) value '502805Prov.Einnahmen CB/FP          '.
              10 pic X(36) value '502900Prov.Einnahmen HYPO/AP        '.
              10 pic X(36) value '502905Prov.Einnahmen HYPO/FP        '.
              10 pic X(36) value '503000Prov.Einnahmen DSL Bank/AP    '.
              10 pic X(36) value '503005Prov.Einnahmen DSL Bank/FP    '.
              10 pic X(36) value '505100Prov.Einnahmen BKK Linde/AP   '.
              10 pic X(36) value '505105Prov.Einnahmen BKK Linde/FP   '.
              10 pic X(36) value '506100Prov.Einnahmen DVAG-Hyp/AP    '.
              10 pic X(36) value '507000Prov.Einnahmen SO.-GES/AP     '.
              10 pic X(36) value '507001Prov.Einnahmen Geiger EM/AP   '.
              10 pic X(36) value '507002Prov.Einnahmen GC&C/AP        '.
              10 pic X(36) value '507005Prov.Einnahmen SO.-GES/FP     '.
              10 pic X(36) value '507006Prov.Einnahmen Geiger EM/FP   '.
              10 pic X(36) value '507007Prov.Einnahmen GC&C/FP        '.
220822        10 pic X(36) value '508072Prov.Einnahmen Geiger USt/AP  '.
220822        10 pic X(36) value '508073Prov.Einnahmen Geiger USt/FP  '.
              10 pic X(36) value '508672Prov.Einnahmen JURADIREKT/AP  '.
              10 pic X(36) value '509092Prov.Einnahmen DV/AP          '.
              10 pic X(36) value '509093Prov.Einnahmen DV/FP          '.
              10 pic X(36) value '509272Prov.Einnahmen Planeth Im/AP  '.
              10 pic X(36) value '509393Prov.Einnahmen Devolo/FP      '.
              10 pic X(36) value '509492Prov.Einnahmen TeleSon/AP     '.
              10 pic X(36) value '509493Prov.Einnahmen TeleSon/FP     '.
              10 pic X(36) value '509692Prov.Einnahmen FingerHaus/AP  '.
              10 pic X(36) value '509872Prov.Einnahmen Cadooz/AP      '.
              10 pic X(36) value '509873Prov.Einnahmen Cadooz/FP      '.
              10 pic X(36) value '509892Prov.Einnahmen Lohnwerk/AP    '.
              10 pic X(36) value '509893Prov.Einnahmen Lohnwerk/FP    '.
              10 pic X(36) value '630001Prov.Ausgaben  JURADIREKT/AP U'.
              10 pic X(36) value '630002Prov.Ausgaben  DV/AP U        '.
              10 pic X(36) value '630003Prov.Ausgaben  DV/FP U        '.
              10 pic X(36) value '630004Prov.Ausgaben  Planeth Im/AP U'.
              10 pic X(36) value '630005Prov.Ausgaben  Devolo/FP U    '.
              10 pic X(36) value '630006Prov.Ausgaben  TeleSon/AP U   '.
              10 pic X(36) value '630007Prov.Ausgaben  TeleSon/FP U   '.
              10 pic X(36) value '630008Prov.Ausgaben  FingerHaus/AP U'.
              10 pic X(36) value '630009Prov.Ausgaben  Cadooz/AP U    '.
              10 pic X(36) value '630010Prov.Ausgaben  Cadooz/FP U    '.
              10 pic X(36) value '630011Prov.Ausgaben  Lohnwerk/AP U  '.
              10 pic X(36) value '630012Prov.Ausgaben  Lohnwerk/FP U  '.
              10 pic X(36) value '630100Prov.Ausgaben  GEDLeben/AP    '.
              10 pic X(36) value '630105Prov.Ausgaben  GEDLeben/FP    '.
              10 pic X(36) value '630200Prov.Ausgaben  GEDVers/AP     '.
              10 pic X(36) value '630205Prov.Ausgaben  GEDVers/FP     '.
              10 pic X(36) value '630300Prov.Ausgaben  GEDKranken/AP  '.
              10 pic X(36) value '630305Prov.Ausgaben  GEDKranken/FP  '.
              10 pic X(36) value '630400Prov.Ausgaben  ADV-RS/AP      '.
              10 pic X(36) value '630405Prov.Ausgaben  ADV-RS/FP      '.
              10 pic X(36) value '631100Prov.Ausgaben  AGI/AP         '.
              10 pic X(36) value '631105Prov.Ausgaben  AGI/FP         '.
              10 pic X(36) value '631700Prov.Ausgaben  DWS/AP         '.
              10 pic X(36) value '631705Prov.Ausgaben  DWS/FP         '.
              10 pic X(36) value '632000Prov.Ausgaben  DB/AP          '.
              10 pic X(36) value '632005Prov.Ausgaben  DB/FP          '.
              10 pic X(36) value '632100Prov.Ausgaben  Santander/AP   '.
              10 pic X(36) value '632105Prov.Ausgaben  Santander/FP   '.
              10 pic X(36) value '632200Prov.Ausgaben  BADENIA/AP     '.
              10 pic X(36) value '632205Prov.Ausgaben  BADENIA/FP     '.
              10 pic X(36) value '632300Prov.Ausgaben  GEDHyp.-Da/AP  '.
              10 pic X(36) value '632305Prov.Ausgaben  GEDHyp.-Da/FP  '.
              10 pic X(36) value '632800Prov.Ausgaben  CB/AP          '.
              10 pic X(36) value '632805Prov.Ausgaben  CB/FP          '.
              10 pic X(36) value '632900Prov.Ausgaben  HYPO/AP        '.
              10 pic X(36) value '632905Prov.Ausgaben  HYPO/FP        '.
              10 pic X(36) value '633000Prov.Ausgaben  DSL Bank/AP    '.
              10 pic X(36) value '633005Prov.Ausgaben  DSL Bank/FP    '.
              10 pic X(36) value '635100Prov.Ausgaben  BKK Linde/AP   '.
              10 pic X(36) value '635105Prov.Ausgaben  BKK Linde/FP   '.
              10 pic X(36) value '636100Prov.Ausgaben  DVAG-Hyp/AP    '.
              10 pic X(36) value '637100Prov.Ausgaben  SO.-GES/AP     '.
              10 pic X(36) value '637101Prov.Ausgaben  Geiger EM/AP   '.
              10 pic X(36) value '637102Prov.Ausgaben  GC&C/AP        '.
              10 pic X(36) value '637105Prov.Ausgaben  SO.-GES/FP     '.
              10 pic X(36) value '637106Prov.Ausgaben  Geiger EM/FP   '.
              10 pic X(36) value '637107Prov.Ausgaben  GC&C/FP        '.
220822        10 pic X(36) value '638072Prov.Ausgaben  Geiger USt/AP K'.
220822        10 pic X(36) value '638073Prov.Ausgaben  Geiger USt/FP K'.
220822        10 pic X(36) value '638074Prov.Ausgaben  Geiger USt/AP U'.
220822        10 pic X(36) value '638075Prov.Ausgaben  Geiger USt/FP U'.
              10 pic X(36) value '638100Erfolgsprovision              '.
              10 pic X(36) value '638108Erfolgsprovision KU           '.
              10 pic X(36) value '638109Erfolgsprovision U            '.
              10 pic X(36) value '638160Existenzgründer-Bonus (Neu)   '.
              10 pic X(36) value '638168Existenzgründer-Bonus (Neu) KU'.
              10 pic X(36) value '638169Existenzgründer-Bonus (Neu) U '.
              10 pic X(36) value '638672Prov.Ausgaben  JURADIREKT/AP K'.
              10 pic X(36) value '638674Prov.Ausgaben  JURADIREKT/AP U'.
              10 pic X(36) value '639092Prov.Ausgaben  DV/AP KU       '.
              10 pic X(36) value '639093Prov.Ausgaben  DV/FP KU       '.
              10 pic X(36) value '639094Prov.Ausgaben  DV/AP U        '.
              10 pic X(36) value '639095Prov.Ausgaben  DV/FP U        '.
              10 pic X(36) value '639272Prov.Ausgaben  Planeth Im/AP K'.
              10 pic X(36) value '639274Prov.Ausgaben  Planeth Im/AP U'.
              10 pic X(36) value '639393Prov.Ausgaben  Devolo/FP KU   '.
              10 pic X(36) value '639395Prov.Ausgaben  Devolo/FP U    '.
              10 pic X(36) value '639492Prov.Ausgaben  TeleSon/AP KU  '.
              10 pic X(36) value '639493Prov.Ausgaben  TeleSon/FP KU  '.
              10 pic X(36) value '639494Prov.Ausgaben  TeleSon/AP U   '.
              10 pic X(36) value '639495Prov.Ausgaben  TeleSon/FP U   '.
              10 pic X(36) value '639692Prov.Ausgaben  FingerHaus/AP K'.
              10 pic X(36) value '639694Prov.Ausgaben  FingerHaus/AP U'.
              10 pic X(36) value '639872Prov.Ausgaben  Cadooz/AP KU   '.
              10 pic X(36) value '639873Prov.Ausgaben  Cadooz/FP KU   '.
              10 pic X(36) value '639874Prov.Ausgaben  Cadooz/AP U    '.
              10 pic X(36) value '639875Prov.Ausgaben  Cadooz/FP U    '.
              10 pic X(36) value '639892Prov.Ausgaben  Lohnwerk/AP KU '.
              10 pic X(36) value '639893Prov.Ausgaben  Lohnwerk/FP KU '.
              10 pic X(36) value '639894Prov.Ausgaben  Lohnwerk/AP U  '.
              10 pic X(36) value '639895Prov.Ausgaben  Lohnwerk/FP U  '.
              10 pic X(36) value '909000EDV-Zwischenkonto Aachen      '.
              10 pic X(36) value '909100EDV-Zwischenkonto Frankfurt   '.
              10 pic X(36) value '909600EDV-Zwischenkonto CPD         '.
              10 pic X(36) value '909700EDV-Zwischenkonto Bestand     '.
              10 pic X(36) value '912000Ausgleich Vertriebsfonds      '.
              10 pic X(36) value '913000Ausgleich Verwaltungsfonds    '.
              10 pic X(36) value '913500Ausgleich USt.-Fonds          '.
              10 pic X(36) value '919000Ausgleich EDV-Konten Allfinanz'.
              10 pic X(36) value '919200Ausgleich Saldovorträge       '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
      *
      * Beginn VG nicht vergeben
      *
            07 TABBEZKTO-WERTE-DUMY.
              10 pic X(36) value '000001Test                          '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
              10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
220822        10 pic X(36) value '999999                              '.
      *
      * Beginn Allfinanz AG
      *
            07 TABBEZKTO-WERTE-ALAG.
              10 pic X(36) value '150100Forderungen GEDLeben          '.
              10 pic X(36) value '150200Forderungen GEDVers           '.
              10 pic X(36) value '150300Forderungen GEDKranken        '.
              10 pic X(36) value '150400Forderungen ADV-RS            '.
              10 pic X(36) value '150910Forderungen Prx Leben         '.
              10 pic X(36) value '150920Forderungen GENVers           '.
              10 pic X(36) value '150925Forderungen GC&C              '.
              10 pic X(36) value '150930Forderungen Gen Inv           '.
              10 pic X(36) value '151100Forderungen AGI               '.
              10 pic X(36) value '151700Forderungen DWS               '.
              10 pic X(36) value '152000Forderungen DB                '.
              10 pic X(36) value '152100Forderungen Santander         '.
              10 pic X(36) value '152200Forderungen BADENIA           '.
              10 pic X(36) value '152800Forderungen CB                '.
              10 pic X(36) value '152900Forderungen HYPO              '.
              10 pic X(36) value '153000Forderungen DSL Bank          '.
              10 pic X(36) value '155100Forderungen BKK Linde         '.
              10 pic X(36) value '156100Forderungen DVAG-Hyp          '.
              10 pic X(36) value '157900Forderungen Geiger EM         '.
220822        10 pic X(36) value '158091Forderungen Geiger USt        '.
              10 pic X(36) value '158691Forderungen JURADIREKT        '.
              10 pic X(36) value '158791Forderungen Cadooz            '.
              10 pic X(36) value '159000Forderungen SO.-GES           '.
              10 pic X(36) value '159091Forderungen DV                '.
              10 pic X(36) value '159291Forderungen Planeth Im        '.
              10 pic X(36) value '159391Forderungen Devolo            '.
              10 pic X(36) value '159491Forderungen TeleSon           '.
              10 pic X(36) value '159691Forderungen FingerHaus        '.
              10 pic X(36) value '159891Forderungen Lohnwerk          '.
              10 pic X(36) value '282700Bank Sammelüberweisung        '.
              10 pic X(36) value '460000Vorsteuer-Konto 19%           '.
              10 pic X(36) value '460310Umsatzsteuer-Konto 19%        '.
              10 pic X(36) value '460412Umsatzsteuer-Konto 16%        '.
              10 pic X(36) value '500001Prov.Einnahmen JURADIREKT/AP  '.
              10 pic X(36) value '500002Prov.Einnahmen DV/AP          '.
              10 pic X(36) value '500003Prov.Einnahmen DV/FP          '.
              10 pic X(36) value '500004Prov.Einnahmen Planeth Im/AP  '.
              10 pic X(36) value '500005Prov.Einnahmen Devolo/FP      '.
              10 pic X(36) value '500006Prov.Einnahmen TeleSon/AP     '.
              10 pic X(36) value '500007Prov.Einnahmen TeleSon/FP     '.
              10 pic X(36) value '500008Prov.Einnahmen FingerHaus/AP  '.
              10 pic X(36) value '500009Prov.Einnahmen Cadooz/AP      '.
              10 pic X(36) value '500010Prov.Einnahmen Cadooz/FP      '.
              10 pic X(36) value '500011Prov.Einnahmen Lohnwerk/AP    '.
              10 pic X(36) value '500012Prov.Einnahmen Lohnwerk/FP    '.
              10 pic X(36) value '500100Prov.Einnahmen GEDLeben/AP    '.
              10 pic X(36) value '500105Prov.Einnahmen GEDLeben/FP    '.
              10 pic X(36) value '500200Prov.Einnahmen GEDVers/AP     '.
              10 pic X(36) value '500205Prov.Einnahmen GEDVers/FP     '.
              10 pic X(36) value '500300Prov.Einnahmen GEDKranken/AP  '.
              10 pic X(36) value '500305Prov.Einnahmen GEDKranken/FP  '.
              10 pic X(36) value '500400Prov.Einnahmen ADV-RS/AP      '.
              10 pic X(36) value '500405Prov.Einnahmen ADV-RS/FP      '.
              10 pic X(36) value '500600Prov.Einnahmen Prx Leben/AP   '.
              10 pic X(36) value '500605Prov.Einnahmen Prx Leben/FP   '.
              10 pic X(36) value '500700Prov.Einnahmen GENVers/AP     '.
              10 pic X(36) value '500705Prov.Einnahmen GENVers/FP     '.
              10 pic X(36) value '501100Prov.Einnahmen AGI/AP         '.
              10 pic X(36) value '501105Prov.Einnahmen AGI/FP         '.
              10 pic X(36) value '501700Prov.Einnahmen DWS/AP         '.
              10 pic X(36) value '501705Prov.Einnahmen DWS/FP         '.
              10 pic X(36) value '501800Prov.Einnahmen Gen Inv/AP     '.
              10 pic X(36) value '501805Prov.Einnahmen Gen Inv/FP     '.
              10 pic X(36) value '502000Prov.Einnahmen DB/AP          '.
              10 pic X(36) value '502005Prov.Einnahmen DB/FP          '.
              10 pic X(36) value '502100Prov.Einnahmen Santander/AP   '.
              10 pic X(36) value '502105Prov.Einnahmen Santander/FP   '.
              10 pic X(36) value '502200Prov.Einnahmen BADENIA/AP     '.
              10 pic X(36) value '502205Prov.Einnahmen BADENIA/FP     '.
              10 pic X(36) value '502800Prov.Einnahmen CB/AP          '.
              10 pic X(36) value '502805Prov.Einnahmen CB/FP          '.
              10 pic X(36) value '502900Prov.Einnahmen HYPO/AP        '.
              10 pic X(36) value '502905Prov.Einnahmen HYPO/FP        '.
              10 pic X(36) value '503000Prov.Einnahmen DSL Bank/AP    '.
              10 pic X(36) value '503005Prov.Einnahmen DSL Bank/FP    '.
              10 pic X(36) value '505100Prov.Einnahmen BKK Linde/AP   '.
              10 pic X(36) value '505105Prov.Einnahmen BKK Linde/FP   '.
              10 pic X(36) value '506100Prov.Einnahmen DVAG-Hyp/AP    '.
              10 pic X(36) value '507000Prov.Einnahmen SO.-GES/AP     '.
              10 pic X(36) value '507001Prov.Einnahmen Geiger EM/AP   '.
              10 pic X(36) value '507002Prov.Einnahmen GC&C/AP        '.
              10 pic X(36) value '507005Prov.Einnahmen SO.-GES/FP     '.
              10 pic X(36) value '507006Prov.Einnahmen Geiger EM/FP   '.
              10 pic X(36) value '507007Prov.Einnahmen GC&C/FP        '.
220822        10 pic X(36) value '508072Prov.Einnahmen Geiger USt/AP  '.
220822        10 pic X(36) value '508073Prov.Einnahmen Geiger USt/FP  '.
              10 pic X(36) value '508672Prov.Einnahmen JURADIREKT/AP  '.
              10 pic X(36) value '509092Prov.Einnahmen DV/AP          '.
              10 pic X(36) value '509093Prov.Einnahmen DV/FP          '.
              10 pic X(36) value '509272Prov.Einnahmen Planeth Im/AP  '.
              10 pic X(36) value '509393Prov.Einnahmen Devolo/FP      '.
              10 pic X(36) value '509492Prov.Einnahmen TeleSon/AP     '.
              10 pic X(36) value '509493Prov.Einnahmen TeleSon/FP     '.
              10 pic X(36) value '509692Prov.Einnahmen FingerHaus/AP  '.
              10 pic X(36) value '509872Prov.Einnahmen Cadooz/AP      '.
              10 pic X(36) value '509873Prov.Einnahmen Cadooz/FP      '.
              10 pic X(36) value '509892Prov.Einnahmen Lohnwerk/AP    '.
              10 pic X(36) value '509893Prov.Einnahmen Lohnwerk/FP    '.
              10 pic X(36) value '630001Prov.Ausgaben  JURADIREKT/AP U'.
              10 pic X(36) value '630002Prov.Ausgaben  DV/AP U        '.
              10 pic X(36) value '630003Prov.Ausgaben  DV/FP U        '.
              10 pic X(36) value '630004Prov.Ausgaben  Planeth Im/AP U'.
              10 pic X(36) value '630005Prov.Ausgaben  Devolo/FP U    '.
              10 pic X(36) value '630006Prov.Ausgaben  TeleSon/AP U   '.
              10 pic X(36) value '630007Prov.Ausgaben  TeleSon/FP U   '.
              10 pic X(36) value '630008Prov.Ausgaben  FingerHaus/AP U'.
              10 pic X(36) value '630009Prov.Ausgaben  Cadooz/AP U    '.
              10 pic X(36) value '630010Prov.Ausgaben  Cadooz/FP U    '.
              10 pic X(36) value '630011Prov.Ausgaben  Lohnwerk/AP U  '.
              10 pic X(36) value '630012Prov.Ausgaben  Lohnwerk/FP U  '.
              10 pic X(36) value '630100Prov.Ausgaben  GEDLeben/AP    '.
              10 pic X(36) value '630105Prov.Ausgaben  GEDLeben/FP    '.
              10 pic X(36) value '630200Prov.Ausgaben  GEDVers/AP     '.
              10 pic X(36) value '630205Prov.Ausgaben  GEDVers/FP     '.
              10 pic X(36) value '630300Prov.Ausgaben  GEDKranken/AP  '.
              10 pic X(36) value '630305Prov.Ausgaben  GEDKranken/FP  '.
              10 pic X(36) value '630400Prov.Ausgaben  ADV-RS/AP      '.
              10 pic X(36) value '630405Prov.Ausgaben  ADV-RS/FP      '.
              10 pic X(36) value '630600Prov.Ausgaben  Prx Leben/AP   '.
              10 pic X(36) value '630605Prov.Ausgaben  Prx Leben/FP   '.
              10 pic X(36) value '630700Prov.Ausgaben  GENVers/AP     '.
              10 pic X(36) value '630705Prov.Ausgaben  GENVers/FP     '.
              10 pic X(36) value '631100Prov.Ausgaben  AGI/AP         '.
              10 pic X(36) value '631105Prov.Ausgaben  AGI/FP         '.
              10 pic X(36) value '631700Prov.Ausgaben  DWS/AP         '.
              10 pic X(36) value '631705Prov.Ausgaben  DWS/FP         '.
              10 pic X(36) value '631800Prov.Ausgaben  Gen Inv/AP     '.
              10 pic X(36) value '631805Prov.Ausgaben  Gen Inv/FP     '.
              10 pic X(36) value '632000Prov.Ausgaben  DB/AP          '.
              10 pic X(36) value '632005Prov.Ausgaben  DB/FP          '.
              10 pic X(36) value '632100Prov.Ausgaben  Santander/AP   '.
              10 pic X(36) value '632105Prov.Ausgaben  Santander/FP   '.
              10 pic X(36) value '632200Prov.Ausgaben  BADENIA/AP     '.
              10 pic X(36) value '632205Prov.Ausgaben  BADENIA/FP     '.
              10 pic X(36) value '632800Prov.Ausgaben  CB/AP          '.
              10 pic X(36) value '632805Prov.Ausgaben  CB/FP          '.
              10 pic X(36) value '632900Prov.Ausgaben  HYPO/AP        '.
              10 pic X(36) value '632905Prov.Ausgaben  HYPO/FP        '.
              10 pic X(36) value '633000Prov.Ausgaben  DSL Bank/AP    '.
              10 pic X(36) value '633005Prov.Ausgaben  DSL Bank/FP    '.
              10 pic X(36) value '635100Prov.Ausgaben  BKK Linde/AP   '.
              10 pic X(36) value '635105Prov.Ausgaben  BKK Linde/FP   '.
              10 pic X(36) value '636100Prov.Ausgaben  DVAG-Hyp/AP    '.
              10 pic X(36) value '637100Prov.Ausgaben  SO.-GES/AP     '.
              10 pic X(36) value '637101Prov.Ausgaben  Geiger EM/AP   '.
              10 pic X(36) value '637102Prov.Ausgaben  GC&C/AP        '.
              10 pic X(36) value '637105Prov.Ausgaben  SO.-GES/FP     '.
              10 pic X(36) value '637106Prov.Ausgaben  Geiger EM/FP   '.
              10 pic X(36) value '637107Prov.Ausgaben  GC&C/FP        '.
220822        10 pic X(36) value '638072Prov.Ausgaben  Geiger USt/AP K'.
220822        10 pic X(36) value '638073Prov.Ausgaben  Geiger USt/FP K'.
220822        10 pic X(36) value '638074Prov.Ausgaben  Geiger USt/AP U'.
220822        10 pic X(36) value '638075Prov.Ausgaben  Geiger USt/FP U'.
              10 pic X(36) value '638100Erfolgsprovision              '.
              10 pic X(36) value '638108Erfolgsprovision KU           '.
              10 pic X(36) value '638109Erfolgsprovision U            '.
              10 pic X(36) value '638160Existenzgründer-Bonus (Neu)   '.
              10 pic X(36) value '638168Existenzgründer-Bonus (Neu) KU'.
              10 pic X(36) value '638169Existenzgründer-Bonus (Neu) U '.
              10 pic X(36) value '638672Prov.Ausgaben  JURADIREKT/AP K'.
              10 pic X(36) value '638674Prov.Ausgaben  JURADIREKT/AP U'.
              10 pic X(36) value '639092Prov.Ausgaben  DV/AP KU       '.
              10 pic X(36) value '639093Prov.Ausgaben  DV/FP KU       '.
              10 pic X(36) value '639094Prov.Ausgaben  DV/AP U        '.
              10 pic X(36) value '639095Prov.Ausgaben  DV/FP U        '.
              10 pic X(36) value '639272Prov.Ausgaben  Planeth Im/AP K'.
              10 pic X(36) value '639274Prov.Ausgaben  Planeth Im/AP U'.
              10 pic X(36) value '639393Prov.Ausgaben  Devolo/FP KU   '.
              10 pic X(36) value '639395Prov.Ausgaben  Devolo/FP U    '.
              10 pic X(36) value '639492Prov.Ausgaben  TeleSon/AP KU  '.
              10 pic X(36) value '639493Prov.Ausgaben  TeleSon/FP KU  '.
              10 pic X(36) value '639494Prov.Ausgaben  TeleSon/AP U   '.
              10 pic X(36) value '639495Prov.Ausgaben  TeleSon/FP U   '.
              10 pic X(36) value '639692Prov.Ausgaben  FingerHaus/AP K'.
              10 pic X(36) value '639694Prov.Ausgaben  FingerHaus/AP U'.
              10 pic X(36) value '639872Prov.Ausgaben  Cadooz/AP KU   '.
              10 pic X(36) value '639873Prov.Ausgaben  Cadooz/FP KU   '.
              10 pic X(36) value '639874Prov.Ausgaben  Cadooz/AP U    '.
              10 pic X(36) value '639875Prov.Ausgaben  Cadooz/FP U    '.
              10 pic X(36) value '639892Prov.Ausgaben  Lohnwerk/AP KU '.
              10 pic X(36) value '639893Prov.Ausgaben  Lohnwerk/FP KU '.
              10 pic X(36) value '639894Prov.Ausgaben  Lohnwerk/AP U  '.
              10 pic X(36) value '639895Prov.Ausgaben  Lohnwerk/FP U  '.
              10 pic X(36) value '909000EDV-Zwischenkonto Aachen      '.
              10 pic X(36) value '909100EDV-Zwischenkonto Frankfurt   '.
              10 pic X(36) value '909600EDV-Zwischenkonto CPD         '.
              10 pic X(36) value '909700EDV-Zwischenkonto Bestand     '.
              10 pic X(36) value '912000Ausgleich Vertriebsfonds      '.
              10 pic X(36) value '913000Ausgleich Verwaltungsfonds    '.
              10 pic X(36) value '913500Ausgleich USt.-Fonds          '.
              10 pic X(36) value '919000Ausgleich EDV-Konten Allf.AG  '.
              10 pic X(36) value '919100Saldenübertrag EVG            '.
              10 pic X(36) value '919200Ausgleich Saldovorträge       '.
      *
           05 redefines TABBEZKTO-WERTE.
              10 TABBEZKTO.
                 15 TABBEZKTO-VG             occurs 5
                                             indexed by TABBEZKTO-IND1.
220822*             20 TABBEZKTO-VG-GES      occurs 186
220822              20 TABBEZKTO-VG-GES      occurs 193
                                             indexed by TABBEZKTO-IND2.
                       25 TABBEZKTO-KONTO    pic 9(6).
                       25 TABBEZKTO-BEZEICH  pic X(30).
