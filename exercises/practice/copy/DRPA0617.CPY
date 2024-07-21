      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0617.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0617.CPv  $
      *
      *   Rev 2.7   Oct 06 2006 12:13:40   t13ex25
      *APA00495: Generali Versicherung
      *
      *   Rev 2.6   25 Sep 2000 12:15:26   T13EX21
      *APA00105 - Neue Funktion DRPA1422
      *
      *   Rev 2.5   30 Aug 2000 11:15:00   T13EX22
      *APA00105: PVW III
      *
      *   Rev 2.4   May 04 2000 10:22:12   T13EX21
      *APA00090 - Neue Funktion
      *
      *   Rev 2.3   Mar 06 2000 14:08:14   T13EX13
      *Anpassung Vorbewegungsermittlung
      *
      *   Rev 2.2   Sep 08 1999 13:35:34   T13EX25
      *FPA00063 Neue Funktion DRPA1299
      *
      *   Rev 2.1   Mar 11 1999 10:34:14   T13EX22
      *MFE-Umstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0617                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0082                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.11.97! 01 ! NEU                       ! PRIGGEN  *
      * FPA00063!25.08.99! 02 ! NEUE FUNKTION DRPA1299    ! JÄGER    *
      * APPA0078!08.02.00! 03 ! WIK VORBEWEGUNG ERMITTELN ! WILKENS  *
      * APA00090!02.05.00! 04 ! NEUE FUNKTION DRPA1373    ! STORCH   *
      * APA00105!07.08.00! 05 ! NEUE FUNKTION DRPA1412    ! MUND     *
      * APA00105!22.09.00! 06 ! NEUE FUNKTION DRPA1422    ! STORCH   *
      * APA00495!02.10.06! 07 ! NEUE FUNKTION DRPA1659    ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-UPPA0082.
           05  PI-UPPA0082-FUNKTION            PIC  X(008).
               88 PI-UPPA0082-DRPA0618              VALUE 'DRPA0618'.
               88 PI-UPPA0082-DRPA0619              VALUE 'DRPA0619'.
               88 PI-UPPA0082-DRPA0620              VALUE 'DRPA0620'.
               88 PI-UPPA0082-DRPA0621              VALUE 'DRPA0621'.
               88 PI-UPPA0082-DRPA0622              VALUE 'DRPA0622'.
               88 PI-UPPA0082-DRPA0623              VALUE 'DRPA0623'.
250899         88 PI-UPPA0082-DRPA1299              VALUE 'DRPA1299'.
080200         88 PI-UPPA0082-DRPA1334              VALUE 'DRPA1334'.
020500         88 PI-UPPA0082-DRPA1373              VALUE 'DRPA1373'.
070800         88 PI-UPPA0082-DRPA1412              VALUE 'DRPA1412'.
220900         88 PI-UPPA0082-DRPA1422              VALUE 'DRPA1422'.
021006         88 PI-UPPA0082-DRPA1659              VALUE 'DRPA1659'.
      *
      ******* ENDE COPY-MEMBER **************************************

