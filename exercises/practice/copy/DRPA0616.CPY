      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0616.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0616.CPv  $
      *
      *   Rev 2.4   Jul 16 2004 10:24:38   t13ex44
      *APA00308: Wiederverwendung Vertrags-Nr. bei der DeuBa
      *
      *   Rev 2.3   Apr 16 2002 14:58:38   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.2   24 May 2000 10:26:04   T13EX23
      *APA00090 - Neue Funktion DRPA1387 
      *
      *   Rev 2.1   Jun 24 1999 16:56:50   T13EX21
      *APA00012 - Neue Funktion
      *
      *   Rev 2.0   May 26 1999 16:05:46   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.1   12 Nov 1998 09:16:52   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:55:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0616                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTPA0106                        *
      *    Standard-OZP                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JäGER-SOKOLUK                                    *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.11.96!01.01! NEU                      ! JÄGER    *
      *         !10.02.98!     ! NEUE FUNKTION DRAT0228   ! STORCH   *
      *         !08.02.99!     ! NEUE FUNKTION DRPA1163   ! LÜTTGES  *
      *         !24.06.99!     ! NEUE FUNKTION DRPA1273   ! STORCH   *
      *         !22.05.00!     ! NEUE FUNKTION DRPA1387   ! GRAY     *
      * FPA00346!11.03.02!2.03 ! NEUE FUNKTION DRPA1532   ! MUND     *
      * APA00308!28.06.04!2.04 ! NEUE FUNKTION DRPA1578   ! SCHEIDEL *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZTPA0106.
           05  PI-ZTPA0106-FUNKTION            PIC  X(008).
               88 PI-ZTPA0106-DRPA0610-SEL-ALL      VALUE 'DRPA0610'.
               88 PI-ZTPA0106-DRPA0789-INS          VALUE 'DRPA0789'.
               88 PI-ZTPA0106-DRPA0790-UPD          VALUE 'DRPA0790'.
               88 PI-ZTPA0106-DRPA0791-DEL          VALUE 'DRPA0791'.
100298         88 PI-ZTPA0106-DRAT0228-COM          VALUE 'DRAT0228'.
080299         88 PI-ZTPA0106-DRPA1163-UPD-PNAP     VALUE 'DRPA1163'.
240699         88 PI-ZTPA0106-DRPA1273-UPD-DGV      VALUE 'DRPA1273'.
220500         88 PI-ZTPA0106-DRPA1387-UPD-PIN      VALUE 'DRPA1387'.
110302         88 PI-ZTPA0106-DRPA1532-DEL-OVW      VALUE 'DRPA1532'.
280604         88 PI-ZTPA0106-DRPA1578-UPD-VNR      VALUE 'DRPA1578'.
