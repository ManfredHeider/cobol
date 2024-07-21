      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA1190.CPY  $
      *    $REVISION:   2.1  $
      *
      *    $LOG:   L:\MF\CPY\DRPA1190.CPV  $
      *
      *   REV 2.1   MAY 27 1999 13:16:58   T01JM2E
      *ÜBERGABE NACH PROD APA00042
      *
      *   REV 2.0   MAY 27 1999 11:46:24   T01JM2E
      *NT-UMSTELLUNG
      *
      *   REV 1.1   APR 15 1999 10:59:08   T01JM2E
      *
      *
      *   REV 1.0   21 OCT 1998 14:04:48   P13SP19
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1190  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    05.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.03.99! 01 ! NEUERSTELLUNG             !D.WILHELM *
      * APA02208!29.01.16!2.2 ! NEU-VERWENDUNG            !BUCH      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1190                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PDPT TBPA0122              *
      *         OHNE UPDATE AUF PDPT-PDPE-TEIL                       *
      ****************************************************************
       01 PI-DRPA1190.
          05 PI-DRPA1190-PDPT-DATEN.
             10 PI-DRPA1190-PDPT-ID              PIC S9(15)V COMP-3.
             10 PI-DRPA1190-PGEB-GES             PIC S9(04) BINARY.
             10 PI-DRPA1190-PDPT-UST-JAHR        PIC S9(04) BINARY.
      *
       01 PO-DRPA1190.
          05 PO-DRPA1190-RC                   PIC  X(003).
             88 PO-DRPA1190-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1190-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1190-RC-NOT-FOUND                  VALUE 'NFD'.
 

