      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0866.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0866.CPv  $
      *
      *   Rev 2.1   18 Sep 2007 08:42:28   t13ex43
      *AVB00941: PGM - Ermitteln VB-Daten für Provision (APA00415)
      *
      *   Rev 2.0   17 Sep 2007 15:56:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0866                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0027                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    17.09.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVB00941!17.09.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0866 - ERMITTELN VB-DATEN FÜR PROVISION       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0866.
           05  PI-DRVB0866-DATEN.
               10  PI-DRVB0866-VBNR              PIC S9(09) COMP.
      *
       01  PO-DRVB0866.
           05  PO-DRVB0866-RC                    PIC  X(03).
               88  PO-DRVB0866-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0866-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0866-DATEN.
               10  PO-DRVB0866-VB-NAME           PIC  X(40).
               10  PO-DRVB0866-VB-VORNAME        PIC  X(20).
               10  PO-DRVB0866-VB-GEBURTSDAT     PIC  X(10).
               10  PO-DRVB0866-VB-PLZ1           PIC  X(09).
               10  PO-DRVB0866-VB-PLZ2           PIC  X(09).
      ****
      **** ENDE COPY DRVB0866 ****************************************
      ****
