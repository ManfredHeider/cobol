      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-429 !19.01.21!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
100521* JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
100521* ------------------------------------------------------------- *
100521* VLA-2586 !10.05.21! FAP-STUFE HINZU UND PVCS-KOPF !           *
100521*          !        ! ENTFERNT                      ! KLEEMEIER *
100521*          !        !                               !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            COPY-MEMBER FÜR CTV-BRIEFE FAP-ANTRAG NEUAUFNAHME  *
      *            DATEINUMMER: CV01240_ VSAM                         *
      *            COPYMEMBER:  DRCV0030                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0030-SATZ.
           02 DRCV0030-VBNR                     PIC 9(007).
           02 DRCV0030-FAP-DATUM                PIC X(010).
           02 DRCV0030-WOCHENTAG                PIC X(010).
           02 DRCV0030-STICHTAG                 PIC X(010).
           02 DRCV0030-GEB-DATUM                PIC X(010).
100521     02 DRCV0030-FAP-STUFE                PIC X(001).
      *
      **** ENDE DRCV0030
      *
