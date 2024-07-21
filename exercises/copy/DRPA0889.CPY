      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0889.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drpa0889.cpv  $
      *
      *   Rev 2.2   Mar 09 2000 17:31:06   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.1   Jan 18 2000 14:37:34   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jan 1998 12:49:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0889                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS BUFFER-OZP ZYPA0033                  *
      *                  FÜR LISTDIALOG SF44                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.11.1999                                       *
      *****************************************************************
      *    VERSION                                                    *
      ******************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA00071!22.11.99! 01 ! NEU                       ! MUND      *
      * APA00087!01.03.00! 02 ! PROD-PGV-ZUORDNUNG AUF    ! WILKENS   *
      *         !        !    ! TBPA0028 UMGESTELLT       !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ******************************************************************
      *
       01  PI-ZYPA0033.
           05  PI-ZYPA0033-FUNKTION            PIC  X(008).
               88 PI-ZYPA0033-DRPA0855              VALUE 'DRPA0855'.
               88 PI-ZYPA0033-DRPA1325              VALUE 'DRPA1325'.
               88 PI-ZYPA0033-DRPA1342              VALUE 'DRPA1342'.
      *
      ******* ENDE COPY-MEMBER *****************************************
