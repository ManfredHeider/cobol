      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0981.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0981.CPv  $
      *
      *   Rev 2.1   Jul 08 2010 15:10:14   t13ex43
      *ARW00069: PGM - Auszahlung GSH umstellen
      *
      *   Rev 2.0   Jun 23 2010 16:12:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0981                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.06.2010                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ARW00069!23.06.10!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0981 - ERMITTELN OFFENE ANSPRÜCHE AUS NKWBA   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0981.
           05  PI-DRVB0981-DATEN.
               10  PI-DRVB0981-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0981-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0981-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0981-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0981-ANSPR-VOM-MM      PIC  9(002).
               10  PI-DRVB0981-KNR-WB-TYP        PIC S9(004) COMP.
      *
       01  PO-DRVB0981.
           05  PO-DRVB0981-RC                    PIC  X(003).
               88  PO-DRVB0981-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0981-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0981-DATEN.
               10  PO-DRVB0981-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0981-KNR-WB-TYP        PIC S9(004) COMP.
               10  PO-DRVB0981-NKWBA-ANSPR-VOM   PIC  X(010).
               10  PO-DRVB0981-NKWBA-PART-VBNR   PIC S9(009) COMP.
               10  PO-DRVB0981-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0981-KTY-WB-TYP        PIC S9(004) COMP.
               10  PO-DRVB0981-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0981-NKWBA-LAST-UPDAT  PIC  X(026).
               10  PO-DRVB0981-NKWBA-ANZ         PIC S9(004) COMP.
               10  PO-DRVB0981-NKWBA-EH-EING     PIC S9(009)V9  COMP-3.
               10  PO-DRVB0981-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0981-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0981-NKWBA-ANSPR-BETR  PIC S9(009)V99 COMP-3.
               10  PO-DRVB0981-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0981-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0981 ****************************************
      ****
