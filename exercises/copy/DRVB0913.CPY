      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0913.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0913.CPv  $
      *
      *   Rev 2.1   Sep 09 2008 12:05:44   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Dialog 1)
      *
      *   Rev 2.0   Aug 01 2008 16:29:46   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0913                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0114                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.08.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00309!01.08.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0913 - ERMITTELN LAUFENDE NUMMER (VSWP) LEIST *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0913.
           05  PI-DRVB0913-DATEN.
               10 PI-DRVB0913-VSWP-LEISTUNG    PIC  X(001).
               10 PI-DRVB0913-VSWP-ART         PIC  X(001).
               10 PI-DRVB0913-KNR-GESELL-TYP   PIC S9(004)      BINARY.
      *
       01  PO-DRVB0913.
           05  PO-DRVB0913-RC                  PIC  X(003).
               88 PO-DRVB0913-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0913-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0913-DATEN.
               10 PO-DRVB0913-VSWP-LFDNR       PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0913 ****************************************
      ****
