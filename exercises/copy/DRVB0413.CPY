      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0413.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0413.CPv  $
      *
      *   Rev 2.1   Jun 04 1999 12:28:34   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 27 1999 13:58:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0413                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS HAPRO PMVB0218 / UU540031/UPOP0009   *
      *    MIT DEN DATEN, DIE EINMAL UEBERGEBEN WERDEN                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    28.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!28.05.99! 01 ! NEUERSTELLUNG             !SCHMIDT,B.*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0413                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      * SATZSTRUKTUR ENTSPRICHT UU010S (ABGESPECKTE VERSION)
      *
       01 DRVB0413-SATZ.
          05 DRVB0413-DATEN.
             10 MMANR                     PIC  9(007) COMP-3.
             10 MAUSDAT                   PIC  9(006) COMP-3.
             10 MSANR                     PIC  9(003) COMP-3.
             10 BETREUER                  OCCURS 12.
                15 MVONDAT                PIC  9(006) COMP-3.
                15 MSTUFE                 PIC  9(003) COMP-3.
                15 MSTATUS                PIC  X(001).
      *
      ***** ENDE COPY-MEMBER DRVB0413 *****************************
