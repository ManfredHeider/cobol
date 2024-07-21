      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0279.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0279.CPv  $
      *
      *   Rev 2.3   Jun 19 2012 09:22:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:47:36   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 08:39:26   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:28:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:42:24   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:29:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0279                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PM INHALT. PRÜFEN   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    28.05.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!31.07.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!11.11.97!    ! KONDITION FÜR GESAMT-     ! WOLFF    *
      *         !        !    ! PROVISION RAUSGENOMMEN    !          *
      * AFPA0012!18.05.98!    ! RC 'MFE' HINZUGEFÜGT      ! PARENT   *
      * AFPA0012!        !    ! CRPA0018                  !          *
      *         !18.09.98!    !   - MELDUNGSFEHLER RAUS-  ! WOLFF    *
      *         !        !    !     GENOMMEN              !          *
      * APA00622!10.08.07!2.01! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA01101!29.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0279                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0279.
          05 PI-DRPA0279-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0279-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA0279.
          05 PO-DRPA0279-RC                         PIC  X(003).
             88 PO-DRPA0279-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0279-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************
