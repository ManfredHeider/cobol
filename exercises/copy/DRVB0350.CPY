      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0350.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0350.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 15:38:42   T13AE35
      *AAS00096 neu
      *
      *   Rev 1.0   27 Oct 1998 16:57:10   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * COPYMEMBER DRVB0350, SATZBESCHREIBUNG FUER DIE DATEI         *
      * VB370100 (NEUE STRUKTURDATEI).                              *
      *                                                              *
      * SATZLAENGE  27 BYTE                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00096!20.01.99! 01 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRVB0350-SATZ.
           05  DRVB0350-KEY.
               10  DRVB0350-BETR-NR      PIC  9(007).
               10  DRVB0350-STUFE        PIC  9(003).
               10  DRVB0350-UNTST-NR     PIC  9(007).
           05  DRVB0350-DAT-VON          PIC  X(010).
