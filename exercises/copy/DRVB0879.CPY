      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0879.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0879.CPv  $
      *
      *   Rev 2.1   14 Jan 2008 13:01:18   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   04 Dec 2007 14:14:14   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0879                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0029                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.12.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!04.12.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0879 - ERMITTELN VB-DATEN FÜR FIBU-SCHNITTST. *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0879.
           05  PI-DRVB0879-DATEN.
               10  PI-DRVB0879-MA-ST-NR          PIC S9(009) COMP.
      *
       01  PO-DRVB0879.
           05  PO-DRVB0879-RC                    PIC  X(003).
               88  PO-DRVB0879-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0879-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0879-DATEN.
               10  PO-DRVB0879-ST-PRUEFZIFFER    PIC  X(001).
               10  PO-DRVB0879-DIR-NR            PIC S9(004) COMP.
               10  PO-DRVB0879-MS-BEZ-KURZ       PIC  X(004).
               10  PO-DRVB0879-MAN-NAME          PIC  X(040).
               10  PO-DRVB0879-MAN-VORNAME       PIC  X(020).
      ****
      **** ENDE COPY DRVB0879 ****************************************
      ****
