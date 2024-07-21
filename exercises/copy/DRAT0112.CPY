      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0112  KONSTANTEN FÜR PGRP-ART   KTY = 102*
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !17.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * AFPA0.. !30.01.97! 02 !PRGP-ART-KG-PEW-EXP    NEU ! BOHLINGER*
      * AAT00001!22.10.97! 03 !ÄNDERUNGEN                 ! HEIDER   *
      * AAT00001!30.01.98! 03 !VERSICHERUNGSSTEUER        ! HEIDER   *
      * AAT00007!10.09.98!1.3 !EURO-PRODUKTE              ! HEIDER   *
      * APA00191!13.06.02!2.3 !BEITRAGSMAXIMIERUNG        ! STORCH   *
      * FPA01082!27.04.09!2.4 !PRODUKTGEBER-GES.91        ! WANG     *
      *         !26.08.09!2.5 !PRODUKTGEBER-GES.91 KTY=361! WANG     *
      * APA01895!06.11.14!2.6 !STORNONEUTRAL AB 36. MONAT ! WANG     *
      * APA02093!05.06.15!2.7 !EINHEITEN BEI DYNAMIK      ! SCHEIDEL *
      * APA02699!27.08.18!2.8 !VM/PS-ZULÄSSIGKEIT         ! WANG     *
      * DSY04062!29.09.21!    !EINHEITEN FUER VWF/VTF     ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PGRP-ART-KONDITION      PIC S9(004) BINARY SYNC
                                                     VALUE  1.
           05 K-KNR-PGRP-ART-WV-DATUM       PIC S9(004) BINARY SYNC
                                                     VALUE  2.
           05 K-KNR-PGRP-ART-WDV-DAUER      PIC S9(004) BINARY SYNC
                                                     VALUE 126.
           05 K-KNR-PGRP-ART-ZWEIGTEXTE     PIC S9(004) BINARY SYNC
                                                     VALUE 127.
           05 K-KNR-PGRP-ART-AEHNL-PROD     PIC S9(004) BINARY SYNC
                                                     VALUE 128.
           05 K-KNR-PGRP-ART-N-DURCH-PROV   PIC S9(004) BINARY SYNC
                                                     VALUE 129.
           05 K-KNR-PGRP-ART-EXPORT-PEW     PIC S9(004) BINARY SYNC
                                                     VALUE 130.
           05 K-KNR-PGRP-ART-VERS-STEUER    PIC S9(004) BINARY SYNC
                                                     VALUE 152.
           05 K-KNR-PGRP-ART-EURO-PRODUKTE  PIC S9(004) BINARY SYNC
                                                     VALUE 168.
           05 K-KNR-PGRP-ART-BEITRAGSMAX    PIC S9(004) BINARY SYNC
                                                     VALUE 204.
           05 K-KNR-PGRP-ART-PRODUKTGEBER   PIC S9(004) BINARY SYNC
      *                                              VALUE 201.
                                                     VALUE 361.
           05 K-KNR-PGRP-ART-ST-NEUTRAL-36M PIC S9(004) BINARY SYNC
                                                     VALUE 394.
           05 K-KNR-PGRP-ART-EINH-B-DYN     PIC S9(004) BINARY SYNC
                                                     VALUE 395.
           05 K-KNR-PGRP-ART-VM-PRD-UNZUL   PIC S9(004) BINARY SYNC
                                                     VALUE 417.
           05 K-KNR-PGRP-ART-EINH-VWF       PIC S9(004) BINARY 
                                                     VALUE 464.
           05 K-KNR-PGRP-ART-EINH-VTF       PIC S9(004) BINARY 
                                                     VALUE 465.


