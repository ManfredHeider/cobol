      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3318.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3318.CPv  $
      *
      *   Rev 2.2   Apr 20 2018 10:26:12   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.1   Apr 20 2018 09:53:24   t13ex78
      *AKI01573: Rück-DTA Verarbeitung
      *
      *   Rev 2.0   Mar 07 2018 16:45:44   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *  Rück-DTA für Generali Altverträge (Rumpfdaten)               *
      *  Betreuerwechsel und Direktionsänderungen inkl. der Gesell-   *
      *  schaftsnummer. Hier zusammengefasst sind:                    *
      *                                                               *
      *  Gesellschaft 11 - Generali Sach                              *
      *  Gesellschaft 21 - Generali Leben                             *
      *  Gesellschaft 42 - Generali Invest                            *
      *                                                               *
      *****************************************************************
      *  Satzlänge 29 Bytes                                           *
      *                                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01573!05.03.18!2.0 ! Neuerstellung             ! tfl       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
 
       01  'XX'-RDTA-BETRW-SATZ.
           05 'XX'-SATZART                    PIC 9(002).
           05 'XX'-GESELLSCHAFT               PIC 9(002).
           05 'XX'-VNR                        PIC X(015).
           05 'XX'-DIRNR                      PIC 9(003).
           05 'XX'-VBNR                       PIC 9(007).
 

