      ******************************************************************
      *    COPY-MEMBER: DRAN0303  FUNKTION INSERT STANDARD-OZP
      *    FÜR DVM-TBAN0027
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01354!04.01.17!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      * AENDERUNGEN:                                                *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME    *
      * ------------------------------------------------------------*
      * ANTRAG-1930!        ! NEU FELD DEVISENINLÄNDER    ! RAHNER  *
      *            !        !                             !         *
      *            !        !                             !         *
      *            !        !                             !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0303                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE INH  TBAN0027              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0303.
           05 PI-DRAN0303-INHA-KEY.
              10 PI-DRAN0303-PGEB-GES          PIC S9(004) COMP.
              10 PI-DRAN0303-INHA-VNR          PIC S9(015) COMP-3.
              10 PI-DRAN0303-VB-NR             PIC S9(009) COMP.
              10 PI-DRAN0303-VM-NR             PIC S9(009) COMP.
              10 PI-DRAN0303-INHABER OCCURS 5.
                 20 PI-DRAN0303-INH-ZUNAME     PIC  X(026).
                 20 PI-DRAN0303-INH-VORNAME    PIC  X(026).
                 20 PI-DRAN0303-INH-STAANG     PIC S9(004) COMP.
                 20 PI-DRAN0303-INH-PLZ        PIC  X(009).
                 20 PI-DRAN0303-INH-ORT        PIC  X(021).
                 20 PI-DRAN0303-INH-AUSLKZ     PIC S9(004) COMP.
                 20 PI-DRAN0303-INH-GEBDAT     PIC  X(010).
                 20 PI-DRAN0303-INH-PEP-STAT   PIC  X(001).
                 20 PI-DRAN0303-INH-DIL-STAT   PIC  X(001).
                 20 PI-DRAN0303-INH-BGRP       PIC S9(004) COMP.
                 20 PI-DRAN0303-INH-BRAN       PIC S9(004) COMP.
      *
       01  PO-DRAN0303.
           05 PO-DRAN0303-RC                        PIC  X(003).
             88 PO-DRAN0303-RC-OK                       VALUE 'OK '.
             88 PO-DRAN0303-RC-DUPLICATE-VALUE          VALUE 'DVA'.



