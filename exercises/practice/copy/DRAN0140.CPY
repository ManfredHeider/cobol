      *****************************************************************
      *    COPY-MEMBER: DRAN0140 FUNKTION                             *
      *                 ERMITTELN WORKFLOW ANTRAG ÜBER VERSNR.        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00655!10.10.06!2.1 ! NEU                       ! HEIDER    *
      * AAN00824!24.11.08!2.2 ! ELEKTR. ANTRAG GES. 16+26 ! RAHNER    *
      * AAN01033!14.12.11!2.10! ERWEITERUNG DRAN0178      ! RAHNER    *
      * AAN01073!25.07.13!2.12! SEPA ÖSTERREICH           ! RAHNER    *
      * AAN01198!05.05.15!2.13! DVM                       ! RAHNER    *
      * AAN01198!25.08.15!2.16! C+L WG. DRAN0178          ! RAHNER    *
      * AAN01240!22.02.16!2.18! NEU DMV: FIRMA-NAME-LANG  ! RAHNER    *
      * AAN01354!20.03.18!2.20! WEITERE INHABNER ANTR. ÖST! RAHNER    *
      * FAN01128!17.04.18!2.23! FALSCHE LÄNGE FILLER      ! RAHNER    *
      * AAN01425!21.08.19!2.24! C+L WG. DRAN0178          ! RAHNER    *
      *         !        !    !                           !           *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME    *
      * ------------------------------------------------------------*
      * ANTRAG-1930!        ! NEU FELD DEVISENINLÄNDER    ! RAHNER  *
      *            !        !                             !         *
      *            !        !                             !         *
      *            !        !                             !         *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
       01 PI-DRAN0140.
          05 ANTA-KEY.
             10 PGEB-GES                         PIC S9(4)   COMP.
             10 ANTA-NR                          PIC S9(15)V COMP-3.
             10 ANTA-AVERM                       PIC S9(9)   COMP.
             10 ANTA-VORSCHLAG-NR                PIC  X(17).
             10 HVANTNUM-LFD                     PIC  X(01).
      *
       01 PO-DRAN0140.
      *      RC ANTRAGSBESTAND
          05 PO-DRAN0140-RC-1                    PIC  X(003).
             88 PO-DRAN0140-RC-1-OK                   VALUE 'OK '.
             88 PO-DRAN0140-RC-1-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0140-RC-1-OK-OHNE-PROD         VALUE 'OOP'.
      *
      *      RC PE-STRUKTUR
          05 PO-DRAN0140-RC-2                    PIC  X(003).
             88 PO-DRAN0140-RC-2-PE-S-OK              VALUE 'PEO'.
             88 PO-DRAN0140-RC-2-PE-S-NOK             VALUE 'PEN'.
      *
      *      RC ELEKTRONISCHE VORSCHLAGSDATEN
          05 PO-DRAN0140-RC-3                    PIC  X(003).
             88 PO-DRAN0140-RC-3-VD-OK                VALUE 'VDO'.
             88 PO-DRAN0140-RC-3-VD-NOK               VALUE 'VDN'.
      *
          05 PO-DRAN0140-DATEN.
             10 PO-DRAN0140-HINWEISTEXT          PIC  X(079).
      *
             10 PO-DRAN0140-MASKENFELDER.
                COPY DRAN0178.
      *
             10 PO-DRAN0140-SONSTIGES.
                20 PO-DRAN0140-DVM-FA-NAME-LANG  PIC X(070).

                20 PO-DRAN0140-INHABER-INFO-KZ   PIC X(001).
                   88 PO-DRAN0140-INHABER-INFO-J     VALUE 'J'.
                   88 PO-DRAN0140-INHABER-INFO-N     VALUE 'N'.

                20 PO-DRAN0140-INHABER-INFO OCCURS 5.
                   30 PO-INHABER-INFO-ZUNAME     PIC  X(026).
                   30 PO-INHABER-INFO-VORNAME    PIC  X(026).
                   30 PO-INHABER-INFO-STAANG     PIC S9(004) COMP.
                   30 PO-INHABER-INFO-PLZ        PIC  X(009).
                   30 PO-INHABER-INFO-ORT        PIC  X(021).
                   30 PO-INHABER-INFO-AUSLKZ     PIC S9(004) COMP.
                   30 PO-INHABER-INFO-GEBDAT     PIC  X(006).
                   30 PO-INHABER-INFO-PEP-STAT   PIC  X(001).
                   30 PO-INHABER-INFO-BGRP       PIC S9(004) COMP.
                   30 PO-INHABER-INFO-BRAN       PIC S9(004) COMP.
                   30 PO-INHABER-INFO-DIL-STAT   PIC  X(001).

                20 PO-DRAN0140-FILLER            PIC  X(0509).
      *