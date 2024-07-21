      *---------------------------------------------------------------*
      *
      *    COPY-MEMBER FUER WEITERE INHABER ANTRAG ÖSTERREICH
      *    --------------------------------------------------
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN01354!20.03.18!2.1  ! NEU                      ! RAHNER   *
      *         !        !     !                          !          *
      * AENDERUNGEN:                                                *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME    *
      * ------------------------------------------------------------*
      * ANTRAG-1930!        ! NEU FELD DEVISENINLÄNDER    ! RAHNER  *
      *            !        !                             !         *
      *            !        !                             !         *
      *            !        !                             !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ***  INHABER DATEN ANTRAG ÖSTERREICH
           05 Z-WEITERE-INHABER-OE.
              10 Z-ZUSATZ-INHA-OE-VB-NR             PIC  9(007) COMP.
              10 Z-ZUSATZ-INHA-OE-VM-NR             PIC  9(007) COMP.
              10 Z-ZUSATZ-INHA-OE-PGEB-GES          PIC S9(004) COMP.
              10 Z-ZUSATZ-INHA-OE-VNR               PIC S9(015) COMP-3.
              10 Z-ZUSATZ-INHA1-OE-INH-ZUNAME       PIC  X(026).
              10 Z-ZUSATZ-INHA1-OE-INH-VORNAME      PIC  X(026).
              10 Z-ZUSATZ-INHA1-OE-INH-STAANG       PIC  X(003).
              10 Z-ZUSATZ-INHA1-OE-INH-PLZ          PIC  X(009).
              10 Z-ZUSATZ-INHA1-OE-INH-ORT          PIC  X(021).
              10 Z-ZUSATZ-INHA1-OE-INH-AUSLKZ       PIC  X(003).
              10 Z-ZUSATZ-INHA1-OE-INH-GEBDAT       PIC  X(006).
              10 Z-ZUSATZ-INHA1-OE-INH-PEP-STAT     PIC  X(001).
              10 Z-ZUSATZ-INHA1-OE-INH-DIL-STAT     PIC  X(001).
              10 Z-ZUSATZ-INHA1-OE-INH-BGRP         PIC  X(030).
              10 Z-ZUSATZ-INHA1-OE-INH-BRAN         PIC  X(030).
              10 ZI-ZUSATZ-INHA-OE-INHABER OCCURS 5.
                 15 ZI-ZUSATZ-INHA-OE-INH-ZUNAME    PIC  X(026).
                 15 ZI-ZUSATZ-INHA-OE-INH-VORNAME   PIC  X(026).
                 15 ZI-ZUSATZ-INHA-OE-INH-STAANG    PIC S9(004) COMP.
                 15 ZI-ZUSATZ-INHA-OE-INH-PLZ       PIC  X(009).
                 15 ZI-ZUSATZ-INHA-OE-INH-ORT       PIC  X(021).
                 15 ZI-ZUSATZ-INHA-OE-INH-AUSLKZ    PIC S9(004) COMP.
                 15 ZI-ZUSATZ-INHA-OE-INH-GEBDAT    PIC  X(006).
                 15 ZI-ZUSATZ-INHA-OE-INH-PEP-STAT  PIC  X(001).
                 15 ZI-ZUSATZ-INHA-OE-INH-DIL-STAT  PIC  X(001).
                 15 ZI-ZUSATZ-INHA-OE-INH-BGRP      PIC S9(004) COMP.
                 15 ZI-ZUSATZ-INHA-OE-INH-BRAN      PIC S9(004) COMP.
              10 ZO-ZUSATZ-INHA-OE-INHABER OCCURS 5.
                 15 ZO-ZUSATZ-INHA-OE-INH-ZUNAME    PIC  X(026).
                 15 ZO-ZUSATZ-INHA-OE-INH-VORNAME   PIC  X(026).
                 15 ZO-ZUSATZ-INHA-OE-INH-STAANG    PIC S9(004) COMP.
                 15 ZO-ZUSATZ-INHA-OE-INH-PLZ       PIC  X(009).
                 15 ZO-ZUSATZ-INHA-OE-INH-ORT       PIC  X(021).
                 15 ZO-ZUSATZ-INHA-OE-INH-AUSLKZ    PIC S9(004) COMP.
                 15 ZO-ZUSATZ-INHA-OE-INH-GEBDAT    PIC  X(006).
                 15 ZO-ZUSATZ-INHA-OE-INH-PEP-STAT  PIC  X(001).
                 15 ZO-ZUSATZ-INHA-OE-INH-DIL-STAT  PIC  X(001).
                 15 ZO-ZUSATZ-INHA-OE-INH-BGRP      PIC S9(004) COMP.
                 15 ZO-ZUSATZ-INHA-OE-INH-BRAN      PIC S9(004) COMP.
              10 Z-ZUSATZ-FILLER                    PIC  X(021).



