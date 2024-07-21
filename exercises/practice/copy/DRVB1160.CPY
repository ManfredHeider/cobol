      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1160.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1160.CPv  $
      *
      *   Rev 2.1   Apr 26 2017 12:13:58   t13ex71
      *ASL00567: Hinzufügen QED in PMRW0043 mit neuen Dateien (BPR)
      *
      *   Rev 2.0   Apr 03 2017 16:48:00   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1160                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.02.2011                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00567!03.04.17!2.0 ! NEU                       ! B.Pretsch *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1160 - ERMITTELN OFFENE ANSPRÜCHE AUS QEDA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1160.
           05  PI-DRVB1160-DATEN.
               10  PI-DRVB1160-UFKT-CODE          PIC  9(002).
               10  PI-DRVB1160-KNR-GESELL-TYP     PIC S9(004) COMP.
               10  PI-DRVB1160-KNR-ANSPR-STATUS   PIC S9(004) COMP.
               10  PI-DRVB1160-ANSPR-VOM-JJJJ     PIC  9(004).
               10  PI-DRVB1160-ANSPR-VOM-MM       PIC  9(002).
      *
       01  PO-DRVB1160.
           05  PO-DRVB1160-RC                     PIC  X(003).
               88  PO-DRVB1160-RC-OK                   VALUE 'OK '.
               88  PO-DRVB1160-RC-EOF                  VALUE 'EOF'.
           05  PO-DRVB1160-DATEN.
               10  PO-DRVB1160-MA-ST-NR           PIC S9(009) COMP.
               10  PO-DRVB1160-ED-ST-NR           PIC S9(009) COMP.
               10  PO-DRVB1160-QEDA-ANSPR-VOM     PIC  X(010).
               10  PO-DRVB1160-KTY-ANSPR-GRUND    PIC S9(004) COMP.
               10  PO-DRVB1160-KNR-ANSPR-GRUND    PIC S9(004) COMP.
               10  PO-DRVB1160-QEDA-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB1160-KTY-WAEHRUNG       PIC S9(004) COMP.
               10  PO-DRVB1160-KNR-WAEHRUNG       PIC S9(004) COMP.
               10  PO-DRVB1160-QEDA-ANSPR-BETRAG  PIC S9(009)V99 COMP-3.
               10  PO-DRVB1160-KTY-ANSPR-STATUS   PIC S9(004) COMP.
               10  PO-DRVB1160-KNR-ANSPR-STATUS   PIC S9(004) COMP.
               10  PO-DRVB1160-QEDA-ANSP-BETR-UST PIC S9(009)V99 COMP-3.
      ****
      **** ENDE COPY DRVB1160 ****************************************
      ****

