      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1044.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1044.CPv  $
      *
      *   Rev 2.2   Feb 09 2012 14:45:46   t13ae14
      *ASL00420: Partnerbonus - Ermittlungsprogramm, Ermittlung, Ei
      *nzelübersichten, Buchungsdaten
      *
      *   Rev 2.1   Feb 03 2012 16:43:36   t13ae14
      *ASL00420: Partnerbonus - Ermittlungsprogramm, Ermittlung, Ei
      *nzelübersichten, Buchungsdaten
      *
      *   Rev 2.0   Feb 03 2012 16:08:56   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1044                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    03.02.2012                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00420!03.02.12!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1044 - ERMITTELN OFFENE ANSPRÜCHE AUS PABOA   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1044.
           05  PI-DRVB1044-DATEN.
             10  PI-DRVB1044-UFKT-CODE         PIC  9(002).
             10  PI-DRVB1044-KNR-GESELL-TYP    PIC S9(004) COMP.
             10  PI-DRVB1044-KNR-ANSPR-STATUS  PIC S9(004) COMP.
             10  PI-DRVB1044-ANSPR-VOM-JJJJ    PIC  9(004).
             10  PI-DRVB1044-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB1044.
           05  PO-DRVB1044-RC                  PIC  X(003).
               88  PO-DRVB1044-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1044-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB1044-DATEN.
             10 PO-DRVB1044-MA-ST-NR           PIC S9(09)       COMP.
             10 PO-DRVB1044-PABOA-UNTST        PIC S9(09)       COMP.
             10 PO-DRVB1044-PABOA-ANSPR-VOM    PIC  X(10).
             10 PO-DRVB1044-KNR-ANSPR-GRUND    PIC S9(04)       COMP.
             10 PO-DRVB1044-KTY-ANSPR-GRUND    PIC S9(04)       COMP.
             10 PO-DRVB1044-PABA-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1044-KTY-WAEHRUNG       PIC S9(04)       COMP.
             10 PO-DRVB1044-KNR-WAEHRUNG       PIC S9(04)       COMP.
             10 PO-DRVB1044-PABA-ANSP-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1044-KTY-ANSPR-STATUS   PIC S9(04)       COMP.
             10 PO-DRVB1044-KNR-ANSPR-STATUS   PIC S9(04)       COMP.
      ****
      **** ENDE COPY DRVB1044 ****************************************
      ****
