      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0899.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0899.CPv  $
      *
      *   Rev 2.1   May 28 2008 13:32:58   t13ex43
      *AAS00756: PGM - Zusatzbonus Allfinanz
      *
      *   Rev 2.0   Apr 14 2008 13:05:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0899                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    14.05.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00756!14.05.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0899 - ERMITTELN OFFENE ANSPRÜCHE AUS ZBOA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0899.
           05  PI-DRVB0899-DATEN.
               10  PI-DRVB0899-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0899-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0899-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0899-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0899-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB0899.
           05  PO-DRVB0899-RC                    PIC  X(003).
               88  PO-DRVB0899-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0899-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0899-DATEN.
               10  PO-DRVB0899-ZBOA-ST-NR        PIC S9(009) COMP.
               10  PO-DRVB0899-ZBOA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0899-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0899-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0899-ZBOA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0899-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0899-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0899-ZBOA-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB0899-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0899-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0899 ****************************************
      ****
