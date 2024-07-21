      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0409.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0409.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:28:40   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:33:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0409                                      *
      *    --------------------------------------------------------   *
      *    PEST-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!28.07.98! 1.1! NEU: KNR-AKZ-PEST         ! PARENT   *
      *         !        !    !                           !          *
      * apa00012!13.08.98! 1.2! neu: Strukturerfassungs   ! Parent   *
      *         !        !    ! und -änderungsdatum und   !          *
      *         !        !    ! Vertrauensmann            !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PEST.                   *
      *                                                              *
      ****************************************************************
      *
             30 PEST-DATEN.
                35 PEST-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PEST-AVERM                       PIC S9(009) BINARY.
                35 PEST-KD-BETR                     PIC S9(009) BINARY.
130898          35 PEST-VM                          PIC S9(009) BINARY.
                35 PEST-VM-GESCHFT-KZ               PIC  X(001).
                35 KNR-PEST-ENTSTEH                 PIC S9(004) BINARY.
                35 PEST-AKZ-EVS                     PIC  X(001).
                35 PEST-AKZ-N-D-PROV                PIC  X(001).
                35 PEST-AKZ-S-BEREIN                PIC  X(001).
                35 PEST-AKZ-KDB-ANPAS               PIC  X(001).
130898          35 PEST-STRUK-AEN-DAT               PIC  X(010).
130898          35 PEST-STRUK-DAT                   PIC  X(010).
                35 PEST-VOR-PEST-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PEST-VSTAT                       PIC  X(004).
                35 PEST-VSTAT-UNR                   PIC S9(004) BINARY.
280798*            NAME BEGINNT NICHT MIT PEST, UM DAMIT ANZUDEUTEN
280798*            DASS DIESES FELD NICHT ZU SPEICHERN IST
280798          35 KNR-AKZ-PEST                     PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
