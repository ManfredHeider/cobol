      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0271.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0271.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:37:52   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:28:46   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0271                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<NPM VERD./PRÜFEN    >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.05.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.05.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!11.11.97!    ! KONDITION FÜR GESAMT-     ! WOLFF    *
      *         !        !    ! PROVISION RAUSGENOMMEN    !          *
      *         !        !    !                           !          *
      * afpa0012!18.05.98!    ! rc 'mfe' eingefügt        ! parent   *
      *         !        !    !                           !          *
      * AFPA0012!        !    ! CRPA0018                  !          *
      *         !18.09.98!    !   - MELDUNGSFEHLER RAUS-  ! WOLFF    *
      *         !        !    !     GENOMMEN              !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0271                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0271.
          05 PI-DRPA0271-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0271-DATEN.
             10 PI-DRPA0271-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PI-DRPA0271-PMPP.
                COPY DRPA0290.
111197*      10 PI-DRPA0271-KOND-GSP.
111197*         COPY DRPA0396.
      *
       01 PO-DRPA0271.
          05 PO-DRPA0271-RC                         PIC  X(003).
             88 PO-DRPA0271-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0271-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA0271-DATEN.
             10 PO-DRPA0271-PMPP.
                COPY DRPA0290.
      *
      ******* ENDE COPY-MEMBER **************************************
