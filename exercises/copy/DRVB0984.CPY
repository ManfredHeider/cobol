      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0984.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0984.CPv  $
      *
      *   Rev 2.1   Mar 17 2011 14:26:14   t13ex43
      *AVB01103: PGM - Auszahlung QEDA über PMRW0018
      *
      *   Rev 2.0   Aug 02 2010 08:57:32   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0984                                      *
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
      * AVB01103!02.02.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0984 - ERMITTELN OFFENE ANSPRÜCHE AUS QEDA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0984.
           05  PI-DRVB0984-DATEN.
               10  PI-DRVB0984-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0984-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0984-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0984-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0984-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0984.
           05  PO-DRVB0984-RC                    PIC  X(003).
               88  PO-DRVB0984-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0984-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0984-DATEN.
               10  PO-DRVB0984-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0984-ED-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0984-QEDA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0984-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0984-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0984-QEDA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0984-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0984-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0984-QEDA-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB0984-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0984-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0984 ****************************************
      ****
