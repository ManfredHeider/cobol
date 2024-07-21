      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1161.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1161.CPv  $
      *
      *   Rev 2.1   Jun 08 2017 15:22:18   t13ex71
      *ASL00569: Förderprovision - Neuer Auftrag mit PMRW0043-Eleme
      *nten (BPR) -> am 12.06.17 nach Prod 
      *
      *   Rev 2.0   Apr 26 2017 14:40:44   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1161                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    04.05.2017                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!05.05.17!2.0 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1161 - ERMITTELN OFFENE ANSPRÜCHE AUS         *
      *    FÖRDERPROVISION (FPRA)                                     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1161.
           05  PI-DRVB1161-DATEN.
               10  PI-DRVB1161-UFKT-CODE          PIC  9(002).
               10  PI-DRVB1161-KNR-GESELL-TYP     PIC S9(004) COMP.
               10  PI-DRVB1161-KNR-ANSPR-STATUS   PIC S9(004) COMP.
               10  PI-DRVB1161-ANSPR-VOM-JJJJ     PIC  9(004).
               10  PI-DRVB1161-ANSPR-VOM-MM       PIC  9(002).
      *
       01  PO-DRVB1161.
           05  PO-DRVB1161-RC                     PIC  X(003).
               88  PO-DRVB1161-RC-OK                   VALUE 'OK '.
               88  PO-DRVB1161-RC-EOF                  VALUE 'EOF'.
           05  PO-DRVB1161-DATEN.
               10  PO-DRVB1161-FPRA-ST-NR         PIC S9(009) COMP.
               10  PO-DRVB1161-FPRA-ANSPR-VOM     PIC  X(010).
               10  PO-DRVB1161-KNR-ANSPR-GRUND    PIC S9(004) COMP.
               10  PO-DRVB1161-KTY-ANSPR-GRUND    PIC S9(004) COMP.
               10  PO-DRVB1161-FPRA-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB1161-KTY-WAEHRUNG       PIC S9(004) COMP.
               10  PO-DRVB1161-KNR-WAEHRUNG       PIC S9(004) COMP.
               10  PO-DRVB1161-FPRA-ANSP-BET-FREI PIC S9(009)V99 COMP-3.
               10  PO-DRVB1161-KTY-ANSPR-STATUS   PIC S9(004) COMP.
               10  PO-DRVB1161-KNR-ANSPR-STATUS   PIC S9(004) COMP.
               10  PO-DRVB1161-FPRA-ANSP-BET-UPFL PIC S9(009)V99 COMP-3.
      ****
      **** ENDE COPY DRVB1161 ****************************************
      ****

