      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0275.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0275.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:41:56   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   13 Jul 1998 09:28:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0275                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    14.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!14.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0275                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0275.
          05 PI-DRAT0275-DUMMY                  PIC  X(001).
      *
       01 PO-DRAT0275.

          05 PO-DRAT0275-RC                     PIC  X(003).
             88 PO-DRAT0275-RC-OK                        VALUE 'OK '.

          05 PO-DRAT0275-DATEN.
             10 PO-DRAT0275-MAX-LOG-TIMESTAMP   PIC  X(026).
      *
      ***** Ende Copy-Member DRAT0275 *****************************
