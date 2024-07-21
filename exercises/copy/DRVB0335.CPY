      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0335.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0335.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Dec 1998 10:54:16   T13AE42
      *AAS00079 - Neuerstellung wg. Delta-Rueckkonvertierung
      *
      *   Rev 1.0   27 Oct 1998 16:35:00   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * COPYMEMBER DRVB0335, SATZBESCHREIBUNG FUER DIE DATEI         *
      * VB370000 (temp. strukturdatei).                              *
      *                                                              *
      * SATZLAENGE  27 BYTE                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00079!03.11.98! 01 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRVB0335-SATZ.
           05  DRVB0335-KEY.
               10  DRVB0335-BETR-NR      PIC  9(007).
               10  DRVB0335-STUFE        PIC  9(003).
               10  DRVB0335-BONUS        PIC  9(003).
               10  DRVB0335-UNTST-NR     PIC  9(007).
           05  DRVB0335-DAT-VON          PIC  X(010).
