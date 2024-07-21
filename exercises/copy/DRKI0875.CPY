      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0875.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0875.cpv  $
      *
      *   Rev 2.1   Jul 27 1999 14:52:18   T13EX41
      *AKI00036
      *- NEUERSTELLUNG
      *
      *   Rev 2.0   Apr 29 1999 13:57:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 8 7 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  DATENSATZBESCHREIBUNG VSA-DATEN INFOPOOL-DATEI                *
      *                                                                *
      *  COPY-MEMBER FÜR STATISTIKPROGRAMME FÜR VSA-DATEN FÜR VER-     *
      *   WALTUNGS-PC                                                  *
      *                                                                *
      *  INHALT: VBNUMMER                                              *
      *          ERSTELLTDATUM                                         *
      *          GESELLSCHAFTSKENNZEICHEN                              *
      *          VB-KENNZEICHEN                                        *
      *          ANZAHL                                                *
      *          WORKSTATIONID                                         *
      *          FILLER                                                *
      *                                                                *
      *    SATZLÄNGE   38                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *23.06.99!NEUERSTELLUNG                          !C.HEINECK! 01  *
      *AKI00036!                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0875.

           05  VBNUMMER                  PIC  9(007).
           05  ERSTELLTDATUM             PIC  9(008).
           05  GES-KZ                    PIC  X(007).
           05  VB-KZ                     PIC  9(001).
           05  ANZAHL                    PIC  9(003).
           05  WORKSTATIONID.
               10  DIREKTIONSNR          PIC  9(003).
               10  RESTNR                PIC  9(004).
           05  SPACE-FILLER              PIC  X(005).
      ******************************************************************
