      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0357.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0357.CPv  $
      *
      *   Rev 2.1   May 27 1999 16:45:38   T13AE42
      *AOP00170 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jan 1999 13:40:38   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0357                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS HAPRO PMVB0216 / UA350               *
      *    MIT DEN DATEN, DIE EINMAL UEBERGEBEN WERDEN                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    27.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00170!27.05.99! 01 ! NEUERSTELLUNG             !SCHMIDT,B.*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0357                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      * SATZSTRUKTUR ENTSPRICHT UU010S (ABGESPECKTE VERSION)
      *
       01 DRVB0357-SATZ.


          05 DRVB0357-DATEN.
             10 MMANR                     PIC  9(007) COMP-3.
             10 MZUNAM                    PIC  X(020).
             10 MVNAME                    PIC  X(020).
             10 MSTRASSE                  PIC  X(020).
             10 MPLZ                      PIC  X(006).
             10 MORT                      PIC  X(016).
             10 MEINDAT                   PIC  9(006) COMP-3.
             10 MAUSDAT                   PIC  9(006) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0357 *****************************

