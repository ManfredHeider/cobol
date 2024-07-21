      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4234.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA4234.CPv  $
      *
      *   Rev 2.5   Oct 19 2015 10:06:12   t13ac19
      *AMA00166: Produktion 10/2015 - VB-Abrechnung - Korrektur DRM
      *A4234
      *
      *   Rev 2.4   Oct 19 2015 09:54:44   t13ac19
      *
      *   Rev 2.3   Oct 16 2015 19:13:12   t13ac19
      *AMA00166: Produktion 10/2015 - VB-Abrechnung - Korrektur DRM
      *A4234
      *
      *   Rev 2.2   Sep 17 2015 18:11:00   t13ac19
      *AMA00164: Produktion 09/2015 - VB-Abrechnung - Korrektur DRM
      *A4234
      *
      *   Rev 2.1   Jul 01 2013 17:23:20   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *     mitCopy: Referenztabelle VB-Stufe - Provisionssatz
      *
      *     SOURCE-NAME      :  DRMA4234
      *     DMR-NAME         :  DG-OV4234
      *
      *---------------------------------------------------------------*
      * Kurzbeschreibung:
      * ================
      *    Diese Copy-Strecke ist die Referenztabelle für die
      *    Zuordnung der VB-Stufe/Praxis-Stufe zum persönlichen
      *    Provisionssatz.
      *
      *
      * Voraussetzungen:
      * ===============
      *    Keine besonderen Erläuterungen notwendig.
      *
      *
      * Funktionale Beschreibung:
      * ========================
      *    In der Tabelle ist festgehalten, welche VB- bzw. Praxis-
      *    Stufe welchen persönlichen Provisionssatz hat.
      *    Da fachlich derzeit zuerst der Inhalt von Praxis-Stufe
      *    für die Suche relevant ist, wurden diese Werte vorangestellt.
      *    Damit wird ein gleichzeitiges Suchen ermöglicht.
      *
      *
      *    Die Abkürzung des Tabellennamens leitet sich wie folgt ab:
      *       RVBP: Referenztabelle VB-Stufe - Provisionssatz
      *             -               --         -
      *
      *
      * Schnittstellen:
      * ==============
      *    Keine besonderen Erläuterungen notwendig.
      *
      *---------------------------------------------------------------*
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00164!17.09.15!2.2  ! Erweiterung um PD         ! O.Wilhelm
      * AMA00166!16.10.15!2.3  ! Korrektur RFT in RF-T     ! O.Wilhelm
      * AMA00166!19.10.15!2.4  ! technische Probleme       ! O.Wilhelm
      *         !        !     ! beim Promote              !
      * AMA00166!19.10.15!2.5  ! technische Probleme       ! O.Wilhelm
      *         !        !     ! beim Promote              !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      *---------------------------------------------------------------*
       01  TABELLE-RVBP.
           05 TABRVBP-WERTE.
              10 TABRVBP-WERT-1        pic X(10) value '0002|10000'.
              10 TABRVBP-WERT-2        pic X(10) value '0003|11000'.
              10 TABRVBP-WERT-3        pic X(10) value '0004|12000'.
              10 TABRVBP-WERT-4        pic X(10) value '0005|12600'.
              10 TABRVBP-WERT-5        pic X(10) value '0006|13200'.
              10 TABRVBP-WERT-6        pic X(10) value '0007|13600'.
              10 TABRVBP-WERT-7        pic X(10) value 'VBAS|04000'.
              10 TABRVBP-WERT-8        pic X(10) value 'VBAK|06000'.
              10 TABRVBP-WERT-9        pic X(10) value 'VBA1|06000'.
              10 TABRVBP-WERT-10       pic X(10) value 'ALP |08000'.
              10 TABRVBP-WERT-11       pic X(10) value 'ALD |09000'.
              10 TABRVBP-WERT-12       pic X(10) value 'AL  |09000'.
              10 TABRVBP-WERT-13       pic X(10) value 'AL-D|09000'.
              10 TABRVBP-WERT-14       pic X(10) value 'AL-K|09000'.
              10 TABRVBP-WERT-15       pic X(10) value 'RGS |10000'.
              10 TABRVBP-WERT-16       pic X(10) value 'GS  |11000'.
              10 TABRVBP-WERT-17       pic X(10) value 'HGS |12000'.
              10 TABRVBP-WERT-18       pic X(10) value 'RD1 |12600'.
              10 TABRVBP-WERT-19       pic X(10) value 'RD2 |13200'.
              10 TABRVBP-WERT-20       pic X(10) value 'D   |14000'.
              10 TABRVBP-WERT-21       pic X(10) value 'BD  |13600'.
              10 TABRVBP-WERT-22       pic X(10) value 'PD  |13600'.
              10 TABRVBP-WERT-23       pic X(10) value 'SD  |00000'.
              10 TABRVBP-WERT-24       pic X(10) value 'ED  |00000'.
              10 TABRVBP-WERT-25       pic X(10) value 'RF-T|00000'.
           05 redefines TABRVBP-WERTE.
              10 TABRVBP                     occurs 25
                                             indexed by TABRVBP-IND.
                 15 TABRVBP-KRIT             pic X(4).
                 15                          pic X.
                 15 TABRVBP-PERS-PROVSATZ    pic 999V99.
 
 

