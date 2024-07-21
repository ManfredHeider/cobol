      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0274.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0274.CPv  $
      *
      *   Rev 2.3   May 18 2017 15:49:54   t13ae46
      *APA02438: Devolo: PEG-Copy JWA 
      *
      *   Rev 2.2   Jul 23 2002 11:18:50   t13ex25
      *APA00207; Neue Funktion zur Prodartermittlung
      *
      *   Rev 2.1   Apr 10 2000 10:23:42   T13EX44
      *Neue Funktion 'DRPA1367'
      *
      *   Rev 2.0   Feb 27 1999 14:28:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:39:40   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:29:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0274                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0035                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00067!07.04.00!2.01! NEUE FUNKTION 'DRPA1367'  ! SCHEIDEL *
      * APA00207!23.07.02!2.02! NEUE FUNKTION 'DRPA1543'  ! Jäger    *
      * APA02438!11.05.17!2.03! NEUE FUNKTION 'DRPA1874'  ! WANG     *
      *         !        !    !                           !          *
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
       01  PI-UPPA0035.
           05  PI-UPPA0035-FUNKTION            PIC  X(008).
               88 PI-UPPA0035-DRPA0275              VALUE 'DRPA0275'.
               88 PI-UPPA0035-DRPA1367              VALUE 'DRPA1367'.
230702         88 PI-UPPA0035-DRPA1543              VALUE 'DRPA1543'.
110517         88 PI-UPPA0035-DRPA1874              VALUE 'DRPA1874'.
      *
      ******* ENDE COPY-MEMBER **************************************

