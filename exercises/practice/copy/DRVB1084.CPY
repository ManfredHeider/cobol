      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1084.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1084.CPv  $
      *
      *   Rev 2.2   26 Jul 2017 18:28:30   t13ex71
      *ASL00567: Hinzufügen Ausbilder-Bonus und Eliteclubbonus in P
      *MRW0043, ZJVB0028, DRRW0005 mit LUCY2-Attributen (BPR)
      *
      *   Rev 2.1   Mar 18 2015 10:07:10   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda
      *- Modul PMRW0018 um Ausbilder-Zuschuss + Elite-Club-Bonus er
      *
      *
      *   Rev 2.0   Mar 05 2015 11:22:26   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1084                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    05.03.2015                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00530!05.03.15!2.1 ! NEU                       ! B.PRETSCH *
      * ASL00567!26.07.17!2.2 ! AZA-BETRAG-UPFL HINZU     ! B.PRETSCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1084 - ERMITTELN OFFENE ANSPRÜCHE AUS         *
      *    AUSBILDER-Bonus (AZA)                                      *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1084.
           05  PI-DRVB1084-DATEN.
               10  PI-DRVB1084-UFKT-CODE         PIC  9(002).
               10  PI-DRVB1084-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB1084-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB1084-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB1084-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB1084.
           05  PO-DRVB1084-RC                    PIC  X(003).
               88  PO-DRVB1084-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1084-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB1084-DATEN.
               10  PO-DRVB1084-AZA-BETR-VBNR     PIC S9(009) COMP.
               10  PO-DRVB1084-AZA-ANSPR-MONAT   PIC  X(010).
               10  PO-DRVB1084-AZA-BETRAG        PIC S9(009)V99 COMP-3.
               10  PO-DRVB1084-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB1084-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB1084-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB1084-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB1084-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB1084-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB1084-AZA-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB1084-AZA-BETRAG-UPFL   PIC S9(009)V99 COMP-3.
      ****
      **** ENDE COPY DRVB1084 ****************************************
      ****
 

