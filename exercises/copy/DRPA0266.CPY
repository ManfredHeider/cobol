      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0266.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0266.CPv  $
      *
      *   Rev 2.2   Jan 04 2000 13:06:18   T13AE18
      * 
      *
      *   Rev 2.1   Jul 26 1999 10:38:06   T13AE34
      *APA00046-Daisy-Batch
      *
      *   Rev 2.0   Feb 27 1999 14:28:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:34:56   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:28:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0266                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0031                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00012!24.06.99!2.1 ! DRPA1266                  ! HEIDER   *
      * APA00068!04.11.99!2.2 ! Neue Fkt. DRPA0733,       ! Bohlinger*
      *         !        !    ! DRPA0738, DRPA0739,       !          *
      *         !        !    ! DRPA0740, DRPA0816        !          *
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
       01  PI-UPPA0031.
           05  PI-UPPA0031-FUNKTION            PIC  X(008).
               88 PI-UPPA0031-DRPA0267              VALUE 'DRPA0267'.
240699         88 PI-UPPA0031-DRPA1266              VALUE 'DRPA1266'.
041199         88 PI-UPPA0031-DRPA0733              VALUE 'DRPA0733'.
041199         88 PI-UPPA0031-DRPA0738              VALUE 'DRPA0738'.
041199         88 PI-UPPA0031-DRPA0739              VALUE 'DRPA0739'.
041199         88 PI-UPPA0031-DRPA0740              VALUE 'DRPA0740'.
041199         88 PI-UPPA0031-DRPA0816              VALUE 'DRPA0816'.
      *
      ******* ENDE COPY-MEMBER **************************************
