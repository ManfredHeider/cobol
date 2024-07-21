      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0419.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0419.CPv  $
      *
      *   Rev 2.3   Jun 18 2015 11:58:30   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System,
      *Progr
      *ammanpassung in PVW wegen UST-Feldern
      *
      *   Rev 2.2   16 May 2007 06:42:08   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.1   17 May 2000 14:50:34   T13EX23
      *APA00090 - PINZ-DATEN
      *
      *   Rev 2.0   May 27 1999 13:36:42   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:34:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0419                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVER ERSTELLEN      >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    23.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.10.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!31.08.98! 1.1! PEW-AVERM UND PEW-VM MIT  ! PARENT   *
      *         !        !    ! ÜBERGEBEN                 !          *
      * APA00090!16.05.00! 2.1! NEUEN UEBERGABEBEREICH    ! GRAY     *
      *         !        !    ! FUER PINZ                 !          *
      * APA01932!18.06.15! 2.2! LUCY: NEUER PI-PARAMETER  ! KÖPRÜCÜ  *
      *         !        !    ! PMPP-PG-UST-SATZ          !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0419                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0419.
          05 PI-DRPA0419-DATEN.
             10 PI-DRPA0419-PMPE.
                COPY DRPA0289.
120407*      10 PI-DRPA0419-PEWA-AVERM              PIC S9(009) BINARY.
120407       10 PI-DRPA0419-ANTA-AVERM              PIC S9(009) BINARY.
120407*      10 PI-DRPA0419-PEWA-VM                 PIC S9(009) BINARY.
120407       10 PI-DRPA0419-ANTA-VM                 PIC S9(009) BINARY.
180615       10 PI-DRPA0419-PMPP-PG-UST-SATZ        PIC S9(003)V9(3)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0419.
          05 PO-DRPA0419-RC                         PIC  X(003).
             88 PO-DRPA0419-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0419-DATEN.
             10 PO-DRPA0419-PVER.
                COPY DRPA0408.
160500       10 PO-DRPA0419-PINZ.
160500          COPY DRPA1385.
      *
      ******* ENDE COPY-MEMBER **************************************

