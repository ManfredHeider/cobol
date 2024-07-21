      ****************************************************************
      *    COPY-MEMBER: DRAT0172  KONSTANTEN F‹R                     *
      *                           VERTRAGS-PRODUKTTEIL-ART KTY = 139 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * Apa00012!22.10.97! NEUERSTELLUNG                  ! STORCH   *
      *         !26.02.99! AUSPR‰GUNGEN GE‰NDERT          !BOHLINGER *
      *         !        !                                !          *
      * APA00012!10.05.99! NEU: PT-AKT-GRUNDPRAEMIE       ! WOLFF    *
      *         !        !                                !          *
      * APA00012!18.05.99! PT-AKT-GRUNDPRAEMIE MIT        ! WOLFF    *
      *         !        ! WERT 4                         !          *
      * DSY04569!20.06.22! NEU: GRUNDPRAEMIE MIT          ! SCHEIDEL *
      *         !        !      VERTEILUNG                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-PT-PROD                 PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PT-GRUNDPRAEMIE         PIC S9(004) BINARY VALUE 2.
           05 K-KNR-PT-MEHRPRAEMIE          PIC S9(004) BINARY VALUE 3.
           05 K-KNR-PT-AKT-GRUNDPRAEMIE     PIC S9(004) BINARY VALUE 4.
           05 K-KNR-PT-GRUNDPR-M-VERTEIL    PIC S9(004) BINARY VALUE 5.
