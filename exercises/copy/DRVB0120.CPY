      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0120.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0120.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   16 Apr 1998 15:13:58   T13AE26
      *AAS00041 - Jahreszahlen in Attributnamen umbenannt
      *
      *   Rev 1.0   Dec 17 1996 15:26:50   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *      VERSION
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ????????!??.??.??!    ! Neu Wettbewerb '97        ! ?        *
      * AAS00041!08.04.98!    ! W.Kast ausschließen und   !          *
      *         !        !    ! Korrektur zu FAS00009     !          *
      *         !        !    ! Jahreszahlen in Attribut- !          *
      *         !        !    ! namen in DRVB0120 umbe-   !          *
      *         !        !    ! nannt                     !T. Moll   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *            COPY-MEMBER ZIELERREICHUNGS-DATEI Ziel-WETTBEWERBE *
      *            DATEINUMMER: VB041100 (z.Zt.)                      *
      *            COPYMEMBER:  DRVB0120                              *
      *            SATZLAENGE:  33 BYTES                              *
      *                                                               *
      *---------------------------------------------------------------*
080498*01  DRVB0120-SATZ.
080498*    05  VBNR                    PIC  9(07).
080498*    05  STUFE-STICHTAG          PIC  X(04).
080498*    05  UMS-96                  PIC S9(07)V9(01)  COMP-3.
080498*    05  UMS-96-MON-SCHNITT      PIC S9(07)V9(01)  COMP-3.
080498*    05  UMS-97-MON-ZIEL         PIC S9(07)V9(01)  COMP-3.
080498*    05  UMS-97-AUFGELAUFEN      PIC S9(07)V9(01)  COMP-3.
080498*    05  ANZ-MON-97              PIC  9(02).

080498*    * Attribute Umbenannt 96 -> VJ
080498*    *                     97 -> WBW
080498 01  DRVB0120-SATZ.
080498     05  VBNR                    PIC  9(07).
080498     05  STUFE-STICHTAG          PIC  X(04).
080498     05  UMS-VJ                  PIC S9(07)V9(01)  COMP-3.
080498     05  UMS-VJ-MON-SCHNITT      PIC S9(07)V9(01)  COMP-3.
080498     05  UMS-WBW-MON-ZIEL        PIC S9(07)V9(01)  COMP-3.
080498     05  UMS-WBW-AUFGELAUFEN     PIC S9(07)V9(01)  COMP-3.
080498     05  ANZ-MON-WBW             PIC  9(02).
      *---------------------------------------------------------------*
