      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0497  FUNKTION                           *
      *                           "ERMITTELN/LESEN KONDITIONEN" -NEU *
      *                                                              *
      *    DIE EINZELNEN FUNKTIONEN :                                *
      *    DRPA0498 - ERMITTELN KONDITION FüR GRUNDPROVISION         *
      *    DRAN0100 - ERMITTELN KONDITION FÜR GP EINGER. GESCHÄFT    *
      *    DRPA0499 - ERMITTELN KONDITION FüR GESAMTPROVISION        *
      *    DRPA0500 - ERMITTELN KONDITION FüR EINHEITEN              *
      *    DRPA0501 - ERMITTELN KONDITION FüR KUNDENBETREUER         *
      *    DRPA0502 - ERMITTELN KONDITION FüR EE-PRüFUNG             *
      *    DRPA0503 - ERMITTELN KONDITION FüR AP-/FP-REGELUNG        *
      *                                                              *
      *    DRPA0504 - LESEN KONDITION FüR GRUNDPROVISION             *
      *    DRPA0505 - LESEN KONDITION FüR GESAMTPROVISION            *
      *    DRPA0506 - LESEN KONDITION FüR EINHEITEN                  *
      *    DRPA0507 - LESEN KONDITION FüR KUNDENBETREUER             *
      *    DRPA0508 - LESEN KONDITION FüR EE-PRüFUNG                 *
      *    DRPA0509 - LESEN KONDITION FüR AP-/FP-REGELUNG            *
      *                                                              *
      *    DRPA0823 - ERMITTELN VERSICHERUNGSSTEUER                  *
      *    DRPA1909 - ERMITTELN VM UMBUCHUNG BEI BESTIMMTEN PRODUKTEN*
      *    DRPA1964 - PRODUKTABHAENGIGE EINHEITEN DES VWF            *
      *    DRPA1965 - PRODUKTABHAENGIGE EINHEITEN DES VTF            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.09.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!19.09.97!01.01!NEUERSTELLUNG             ! BOHLINGER*
      *         !31.10.97!01.02!NEUE FUNKTIONEN DRPA0581  !          *
      *         !        !     !DRPA0582                  !BOHLINGER *
      *         !02.02.98!01.03!NEUE FUNKTIONEN DRPA0823  !BOHLINGER *
      * APA00191!13.06.02!02.01!NEUE FUNKTION DRPA1541    ! STORCH   *
      * AAN00369!10.09.03! 2.2 !NEUE FUNKTION DRAN0100    !HEIDER    *
      * APA00725!19.01.09! 2.3 !NEUE FUNKTION DRPA1687    !SCHEIDEL  *
      * FPA01082!23.04.09! 2.4 !NEUE FUNKTION DRPA1679    !WANG      *
      * APA01526!27.07.13! 2.5 !NEUE FUNKTION DRPA1733    !RAHNER    *
      * APA01895!06.11.14! 2.6 !NEUE FUNKTION DRPA1799    !WANG      *
      * APA02093!05.06.15! 2.7 !NEUE FUNKTION DRPA1819    !SCHEIDEL  *
      * APA0nnnn!nn.nn.nn! 2.8 !NEUE FUNKTION DRPA1818    !STORCH    *
      * APA02699!24.08.18! 2.9 !NEUE FUNKTION DRPA1909    !WANG      *
      * DSY04062!29.09.21!     !NEUE DRPA1964 & DRPA1965  !SCHEIDEL  *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0065.
           05 PI-UPPA0065-FUNKTION            PIC X(008).
              88 PI-UPPA0065-DRPA0498             VALUE 'DRPA0498'.
              88 PI-UPPA0065-DRPA0499             VALUE 'DRPA0499'.
              88 PI-UPPA0065-DRPA0500             VALUE 'DRPA0500'.
              88 PI-UPPA0065-DRPA0501             VALUE 'DRPA0501'.
              88 PI-UPPA0065-DRPA0502             VALUE 'DRPA0502'.
              88 PI-UPPA0065-DRPA0503             VALUE 'DRPA0503'.
              88 PI-UPPA0065-DRPA0504             VALUE 'DRPA0504'.
              88 PI-UPPA0065-DRPA0505             VALUE 'DRPA0505'.
              88 PI-UPPA0065-DRPA0506             VALUE 'DRPA0506'.
              88 PI-UPPA0065-DRPA0507             VALUE 'DRPA0507'.
              88 PI-UPPA0065-DRPA0508             VALUE 'DRPA0508'.
              88 PI-UPPA0065-DRPA0509             VALUE 'DRPA0509'.
              88 PI-UPPA0065-DRPA0581             VALUE 'DRPA0581'.
              88 PI-UPPA0065-DRPA0582             VALUE 'DRPA0582'.
              88 PI-UPPA0065-DRPA0588             VALUE 'DRPA0588'.
              88 PI-UPPA0065-DRPA0589             VALUE 'DRPA0589'.
              88 PI-UPPA0065-DRPA0823             VALUE 'DRPA0823'.
              88 PI-UPPA0065-DRPA1541             VALUE 'DRPA1541'.
              88 PI-UPPA0065-DRAN0100             VALUE 'DRAN0100'.
              88 PI-UPPA0065-DRPA1687             VALUE 'DRPA1687'.
              88 PI-UPPA0065-DRPA1679             VALUE 'DRPA1679'.
              88 PI-UPPA0065-DRPA1733             VALUE 'DRPA1733'.
              88 PI-UPPA0065-DRPA1799             VALUE 'DRPA1799'.
              88 PI-UPPA0065-DRPA1818             VALUE 'DRPA1818'.
              88 PI-UPPA0065-DRPA1819             VALUE 'DRPA1819'.
              88 PI-UPPA0065-DRPA1909             VALUE 'DRPA1909'.
290921        88 PI-UPPA0065-DRPA1964             VALUE 'DRPA1964'.
290921        88 PI-UPPA0065-DRPA1965             VALUE 'DRPA1965'.

      ****************************************************************
      * ENDE DRPA0497
      ****************************************************************


