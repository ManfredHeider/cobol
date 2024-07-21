      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0936.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\drpa0936.cpv  $
      *
      *   Rev 2.3   May 17 1999 18:32:50   T01JM2S
      *APA00012 - NEUE FUNKTIONEN DRPA1237, DRPA1238
      *
      *   Rev 2.2   May 06 1999 08:47:14   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1225
      *
      *   Rev 2.1   May 04 1999 16:00:34   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1210
      *
      *   Rev 2.0   Apr 09 1999 09:50:30   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:16:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0936                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0100                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.04.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!29.10.98!02.0! NEU: PI-UPPA0100-DRPA1108 ! WOLFF    *
      *         !        !    !      PI-UPPA0100-DRPA1109 !          *
      *         !04.11.98!    ! NEU: PI-UPPA0100-DRPA1114 ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!04.03.99!2.1 ! NEU: PI-UPPA0100-DRPA1210 ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!05.05.99!2.2 ! NEU: PI-UPPA0100-DRPA1225 ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!14.05.99!2.3 ! NEU: PI-UPPA0100-DRPA1237 ! WOLFF    *
      *         !        !    !      PI-UPPA0100-DRPA1238 !          *
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
       01  PI-UPPA0100.
           05  PI-UPPA0100-FUNKTION            PIC  X(008).
               88 PI-UPPA0100-DRPA0937              VALUE 'DRPA0937'.
               88 PI-UPPA0100-DRPA0938              VALUE 'DRPA0938'.
               88 PI-UPPA0100-DRPA0939              VALUE 'DRPA0939'.
               88 PI-UPPA0100-DRPA0945              VALUE 'DRPA0945'.
               88 PI-UPPA0100-DRPA0951              VALUE 'DRPA0951'.
291098         88 PI-UPPA0100-DRPA1108              VALUE 'DRPA1108'.
291098         88 PI-UPPA0100-DRPA1109              VALUE 'DRPA1109'.
041198         88 PI-UPPA0100-DRPA1114              VALUE 'DRPA1114'.
090499         88 PI-UPPA0100-DRPA1210              VALUE 'DRPA1210'.
050599         88 PI-UPPA0100-DRPA1225              VALUE 'DRPA1225'.
140599         88 PI-UPPA0100-DRPA1237              VALUE 'DRPA1237'.
140599         88 PI-UPPA0100-DRPA1238              VALUE 'DRPA1238'.
      *
      ******* ENDE COPY-MEMBER **************************************
