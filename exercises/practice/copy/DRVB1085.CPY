      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1085.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1085.CPv  $
      *
      *   Rev 2.3   26 Jul 2017 18:28:38   t13ex71
      *ASL00567: Hinzufügen Ausbilder-Bonus und Eliteclubbonus in P
      *MRW0043, ZJVB0028, DRRW0005 mit LUCY2-Attributen (BPR)
      *
      *   Rev 2.2   Apr 29 2015 15:51:40   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda
      *- Korrektur PMRW0018 für ECBA
      *
      *   Rev 2.1   Mar 18 2015 10:07:14   t13ex71
      *ASL00530: Änderung Sonderleistungen 2015 / Wachstums-Agenda
      *- Modul PMRW0018 um Ausbilder-Zuschuss + Elite-Club-Bonus er
      *
      *
      *   Rev 2.0   Mar 05 2015 11:22:34   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1085                                      *
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
      * ASL00567!26.07.17!2.2 ! LUCY2-ATTRIBUTE HINZU     ! B.PRETSCH *
120522* VLA-4763!12.05.22!2.3 ! Felddefinitionen COMP-3   ! U. Heim  *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1085 - ERMITTELN OFFENE ANSPRÜCHE AUS         *
      *    ELITE-CLUB-BONUS (ECBA)                                    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1085.
           05  PI-DRVB1085-DATEN.
               10  PI-DRVB1085-UFKT-CODE          PIC  9(002).
               10  PI-DRVB1085-KNR-GESELL-TYP     PIC S9(004) COMP.
               10  PI-DRVB1085-KNR-ANSPR-STATUS   PIC S9(004) COMP.
               10  PI-DRVB1085-ANSPR-VOM-JJJJ     PIC  9(004).
               10  PI-DRVB1085-ANSPR-VOM-MM       PIC  9(002).
      *
       01  PO-DRVB1085.
           05  PO-DRVB1085-RC                     PIC  X(003).
               88  PO-DRVB1085-RC-OK                   VALUE 'OK '.
               88  PO-DRVB1085-RC-EOF                  VALUE 'EOF'.
           05  PO-DRVB1085-DATEN.
               10  PO-DRVB1085-ECBA-ST-NR         PIC S9(009) COMP.
               10  PO-DRVB1085-ECBA-ANSPR-VOM     PIC  X(010).
               10  PO-DRVB1085-KNR-ANSPR-GRUND    PIC S9(004) COMP.
               10  PO-DRVB1085-KTY-ANSPR-GRUND    PIC S9(004) COMP.
               10  PO-DRVB1085-ECBA-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB1085-KTY-WAEHRUNG       PIC S9(004) COMP.
               10  PO-DRVB1085-KNR-WAEHRUNG       PIC S9(004) COMP.
120522         10  PO-DRVB1085-ECBA-PART-EH       PIC S9(010)V9 COMP-3.
               10  PO-DRVB1085-ECBA-FAKTOR        PIC S9(003)V99 COMP-3.
               10  PO-DRVB1085-ECBA-ANSPR-BETRAG  PIC S9(009)V99 COMP-3.
               10  PO-DRVB1085-KTY-ANSPR-STATUS   PIC S9(004) COMP.
               10  PO-DRVB1085-KNR-ANSPR-STATUS   PIC S9(004) COMP.
120522         10  PO-DRVB1085-ECBA-PART-EH-UPFL  PIC S9(010)V9 COMP-3.
120522         10  PO-DRVB1085-ECBA-EH-UFREI-RECH PIC S9(010)V9 COMP-3.
120522         10  PO-DRVB1085-ECBA-EH-UPFL-RECH  PIC S9(010)V9 COMP-3.
               10  PO-DRVB1085-ECBA-BETRAG-UPFL   PIC S9(009)V99 COMP-3.
      ****
      **** ENDE COPY DRVB1085 ****************************************
      ****
