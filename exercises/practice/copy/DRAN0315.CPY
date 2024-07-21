      *****************************************************************
      *    COPY-MEMBER: DRAN0315                                      *
      *    --------------------------------------------------------   *
      *    ZWISCHENSPEICHER WEITERE INHABER ANTRAG ÖSTERREICH         *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01354!20.03.18!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
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
       20  ZUSATZ-INHABER-SEICHERN-NOTW        PIC  X(001).
           88 Z-INHABER-SPEICHERN-NOTW-NEIN         VALUE '1'.
           88 Z-INHABER-SPEICHERN-NOTW-JA           VALUE '2'.
       20  ZUSATZ-INHABER-OE-VB-NR             PIC  9(007) COMP.
       20  ZUSATZ-INHABER-OE-VM-NR             PIC  9(007) COMP.
       20  ZUSATZ-INHABER-OE-VNR               PIC  9(015) COMP-3.
       20  ZUSATZ-INHABER-OE-INHABER OCCURS 5.
           25 ZUSATZ-INHABER-OE-INH-ZUNAME     PIC  X(026).
           25 ZUSATZ-INHABER-OE-INH-VORNAME    PIC  X(026).
           25 ZUSATZ-INHABER-OE-INH-STAANG     PIC S9(004) COMP.
           25 ZUSATZ-INHABER-OE-INH-PLZ        PIC  X(009).
           25 ZUSATZ-INHABER-OE-INH-ORT        PIC  X(021).
           25 ZUSATZ-INHABER-OE-INH-AUSLKZ     PIC S9(004) COMP.
           25 ZUSATZ-INHABER-OE-INH-GEBDAT     PIC  X(006).
           25 ZUSATZ-INHABER-OE-INH-PEP-STAT   PIC  X(001).
           25 ZUSATZ-INHABER-OE-INH-DIL-STAT   PIC  X(001).
           25 ZUSATZ-INHABER-OE-INH-BGRP       PIC S9(004) COMP.
           25 ZUSATZ-INHABER-OE-INH-BRAN       PIC S9(004) COMP.
       20  ZUSATZ-INHABER-OE-FILLER            PIC  X(027).



