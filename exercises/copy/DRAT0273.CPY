      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0273.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0273.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:41:50   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   13 Jul 1998 09:28:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0273                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    13.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0273                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0273.
          05 PI-DRAT0273-PROGRAMM           PIC  X(008).
          05 PI-DRAT0273-UFKT-CODE          PIC  9(002).
          05 PI-DRAT0273-LAST-UPDATE-VB-SYS         PIC  X(026).
          05 PI-DRAT0273-MAX-LOG-TIMESTAMP          PIC  X(026).
      *
       01 PO-DRAT0273.

          05 PO-DRAT0273-RC                 PIC  X(003).
             88 PO-DRAT0273-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0273-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0273-RC-EOF                       VALUE 'EOF'.

          05 PO-DRAT0273-DATEN.
             10 PO-DRAT0273-LOG-MA-NR       PIC  9(7)    COMP-3.
             10 PO-DRAT0273-LOG-TABLE-ID    PIC  x(008).
      *
      ***** Ende Copy-Member DRAT0273 *****************************
