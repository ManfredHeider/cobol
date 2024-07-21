      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3035.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3035.CPv  $
      *
      *   Rev 2.2   Oct 28 2015 17:20:44   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Ne
      *ues Kü-Ticket 979
      *
      *   Rev 2.1   Oct 28 2015 09:15:14   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Ne
      *ues Kü-Ticket 979
      *
      *   Rev 2.0   Sep 16 2015 09:35:24   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01382!16.09.15!2.0 ! NEU                       !Flierl     *
      * --------------------------------------------------------------*
      *         !        !    !                           !
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI3035 - KUNDENÜBERTRAGUNG MIT TICKET-ID (CoC)  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
 
       01  PI-DRKI3035.
           05  PI-DRKI3035-DATEN.
               10  PI-DRKI3035-OTTICKETID        PIC 9(018) COMP-3.
               10  PI-DRKI3035-OTHOSTKUNDENID    PIC  X(013).
               10  PI-DRKI3035-OTERSTELLT        PIC  X(026).
               10  PI-DRKI3035-OTERSTELLTVON     PIC  X(030).
               10  PI-DRKI3035-OTAENDGRUND-KNR   PIC S9(004) COMP.
 
 
       01  PO-DRKI3035.
           05  PO-DRKI3035-RC                    PIC  X(003).
               88  PO-DRKI3035-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3035-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI3035-DATEN.
               10  PO-DRKI3035-OTFEHLER-KNR      PIC S9(004) COMP.
 
 

