      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2005.CPv  $
      *
      *   Rev 2.1   15 Mar 2007 10:27:28   t13ex43
      *AKI00557: Ticketverfahren Kundenübertragung
      *
      *   Rev 2.0   Jan 08 2007 12:28:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2005                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJKI0030                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.01.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00557!08.01.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI2005 - ERMITTELN ALLE VT EINES KD FÜR EINEN VB*
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI2005.
           05  PI-DRKI2005-DATEN.
               10  PI-DRKI2005-UFKT-CODE         PIC  9(002).
               10  PI-DRKI2005-VRBKIPERSONENID   PIC  X(013).
               10  PI-DRKI2005-VRBVBPERSONENID   PIC  X(013).
               10  PI-DRKI2005-VRBVERTRAGSID-VON PIC  X(013).
               10  PI-DRKI2005-VRBVERTRAGSID-BIS PIC  X(013).
      *
       01  PO-DRKI2005.
           05  PO-DRKI2005-RC                    PIC  X(003).
               88  PO-DRKI2005-RC-OK                  VALUE 'OK '.
               88  PO-DRKI2005-RC-EOF                 VALUE 'EOF'.
           05  PO-DRKI2005-DATEN.
               10  PO-DRKI2005-VRBVERTRAGSID     PIC  X(013).
      ****
      **** ENDE COPY DRKI2005 ****************************************
      ****
