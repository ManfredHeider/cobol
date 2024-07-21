      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0032.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DSKI0032.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   29 Oct 1998 17:51:08   T13EX19
      *FKI00010 - Korrektion
      *
      *   Rev 1.1   12 Aug 1998 16:56:00   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   30 Jun 1998 10:23:06   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DSKI0032                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LAND Supporttabelle                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGUR-VUJIC                                       *
      *    DATUM:    30.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!22.06.98! 01 ! NEUERSTELLUNG             !ZGUR-VUJIC*
      *         !27.10.98! 02 ! aus '999' wird '?  '      !ZGUR-VUJIC*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      ****************************************************************
      *
          05 K-DEUTSCHLAND              PIC  X(003) VALUE 'D  '.
          05 K-FRANKREICH               PIC  X(003) VALUE 'F  '.
          05 K-POLEN                    PIC  X(003) VALUE 'PL '.
          05 K-OESTERREICH              PIC  X(003) VALUE 'A  '.
          05 K-ENGLAND                  PIC  X(003) VALUE 'GB '.
          05 K-NIEDERLANDE              PIC  X(003) VALUE 'NL '.
          05 K-BELGIEN                  PIC  X(003) VALUE 'B  '.
          05 K-SCHWEIZ                  PIC  X(003) VALUE 'CH '.
          05 K-PORTUGAL                 PIC  X(003) VALUE 'P  '.
          05 K-ITALIEN                  PIC  X(003) VALUE 'I  '.
          05 K-SPANIEN                  PIC  X(003) VALUE 'E  '.
          05 K-DAENEMARK                PIC  X(003) VALUE 'DK '.
271098*   05 K-LAND-UNBEKANNT           PIC  X(003) VALUE '999'.
271098    05 K-LAND-UNBEKANNT           PIC  X(003) VALUE '?  '.
      *
      ***** ENDE COPY-MEMBER DSKI0032 *****************************
