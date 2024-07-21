      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3627.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3627.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 10:35:38   t13ex78
      *AKI01669: Ticketverarbeitung und Abgleich
      *
      *   Rev 2.1   Mar 19 2019 13:17:58   t13ex78
      *AKI01669: Ticketverarbeitung und Abgleich
      *
      *   Rev 2.0   Mar 15 2019 09:13:34   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01669!14.03.19!2.0 ! Neuerstellung             ! tfl       *
      * --------------------------------------------------------------*
      *         !        !    !                           !
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI3627 - Kundendaten der DSL Bank (G67)         *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
 
       01  PI-DRKI3627.
           05  PI-DRKI3627-DATEN.
               10  PI-DRKI3627-OTTICKETID        PIC 9(018) COMP-3.
               10  PI-DRKI3627-OTHOSTKUNDENID    PIC  X(013).
               10  PI-DRKI3627-OTERSTELLT        PIC  X(026).
               10  PI-DRKI3627-OTERSTELLTVON     PIC  X(030).
               10  PI-DRKI3627-OTAENDGRUND-KNR   PIC S9(004) COMP.
 
 
       01  PO-DRKI3627.
           05  PO-DRKI3627-RC                    PIC  X(003).
               88  PO-DRKI3627-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3627-RC-NOK                 VALUE 'NOK'.
           05  PO-DRKI3627-DATEN.
               10  PO-DRKI3627-OTFEHLER-KNR      PIC S9(004) COMP.
 
 
 


