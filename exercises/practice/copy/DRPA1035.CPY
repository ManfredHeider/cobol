      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA1035.CPY  $
      *    $REVISION:   2.3  $
      *
      *    $LOG:   L:/MF/CPY/DRPA1035.CPV  $
      *
      *   REV 2.3   JUL 16 2015 15:08:46   T13AE37
      *APA01932: ABU / SCHNITTSTELLEN UND REFERENZ
      *
      *   REV 2.2   16 MAY 2007 06:44:48   T13EX44
      *APA00604: ANPASSUNGEN IN DAISY ZUR ABLÖSUNG PEW
      *
      *   REV 2.1   MAR 16 1999 14:02:58   T01JM2S
      *
      *
      *   REV 2.0   MAR 12 1999 13:54:24   T01JM2S
      *NT-UMSTELLUNG
      *
      *   REV 1.0   14 JUL 1998 14:00:12   P13SP19
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1035                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERSTELLEN PAMS      >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.08.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!28.10.98!    ! NEUES ATTRIBUT PDTA-ID    ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!08.03.99!2.1 ! NEUES ATTRIBUT PEWA-ID    ! WOLFF    *
      * APA00604!12.04.07!2.2 ! PEWA-ID ENTFERNT          ! SCHEIDEL *
      * APA01932!23.06.15!2.3 ! NEUES COPY ABRECH-NR      ! BUCH     *
      * APA01932!14.09.15!    ! KORREKTUR-ZIELSTATUS      ! BUCH     *
      * APA02208!01.02.16!2.4 ! NEUES FLAG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1035                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1035.
          05 PI-DRPA1035-DATEN.
             10 PI-DRPA1035-UFKT                    PIC  9(002).
             10 PI-DRPA1035-EVS-VNR                 PIC  X(015).
281098       10 PI-DRPA1035-PDTA-ID                 PIC S9(015)
281098                                                  PACKED-DECIMAL.
120407*      10 PI-DRPA1035-PEWA-ID                 PIC S9(015)
120407*                                                 PACKED-DECIMAL.
             10 PI-DRPA1035-PVER.
                COPY DRPA0408.
             10 PI-DRPA1035-PVPT.
                COPY DRPA0420.
             10 PI-DRPA1035-PEST-PEBD.
                COPY DRPA0587.
             10 PI-DRPA1035-PMBE.
                COPY DRPA0411.
             10 PI-DRPA1035-PMPR.
                COPY DRPA0432.
             10 PI-DRPA1035-PMPT.
                COPY DRPA0421.
230615       10 PI-DRPA1035-PABR.
230615          COPY DRPA1820.
140915       10 PI-DRPA1035-KORR-ZIEL-STAT          PIC  X(001).
010216       10 PI-DRPA1035-PAMS-UST-JAHR-KZ        PIC  X(001).
      *
       01 PO-DRPA1035.
          05 PO-DRPA1035-RC                         PIC  X(003).
             88 PO-DRPA1035-RC-OK                         VALUE 'OK '.
      *
      ******* ENDE COPY-MEMBER **************************************
 

