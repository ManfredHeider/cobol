      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3519.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3519.CPv  $
      *
      *   Rev 2.3   Nov 29 2018 08:56:38   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI 
      *
      *   Rev 2.2   Nov 28 2018 16:50:22   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI
      *
      *   Rev 2.1   Nov 27 2018 11:59:42   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI
      *
      *   Rev 2.0   Nov 06 2018 09:42:00   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3040                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3040                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3040
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3519.
           05  PI-DRKI3519-PROGRAMM     PIC X(008).
           05  PI-DRKI3519-UFKT-CODE    PIC 9(002).
 
           05  PI-DRKI3519-DATEN.
             10 PI-DRKI3519-LENGTH      PIC S9(4) USAGE COMP.

       01  PO-DRKI3519.
           05  PO-DRKI3519-RC                     PIC  X(003).
              88 PO-DRKI3519-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3519-RC-LEER                     VALUE 'LER'.
              88 PO-DRKI3519-RC-EOF                      VALUE 'EOF'.
      *
           05 PO-DRKI3519-DATEN.
              10 PO-DRKI3519-SYSTABLES-NAME       PIC  X(128).
              10 PO-DRKI3519-SYSCOLUMNS-NAME      PIC  X(128).
      *
      ******* ENDE COPY-MEMBER ***************************************


