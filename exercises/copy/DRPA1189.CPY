      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1189.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1189.CPv  $
      *
      *   Rev 2.5   Jun 09 2020 11:30:52   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.4   Jun 19 2012 09:28:36   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:53:24   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 12:30:28   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Mar 16 1999 14:07:18   T01JM2S
      *
      *
      *   Rev 2.0   Mar 05 1999 10:35:44   T01JM2S
      *
      *
      *   Rev 1.0   21 Oct 1998 14:04:42   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1189                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN FÜR DRPA1181  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    08.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA00622!29.10.07!2.2 ! ERWEITERUNG UM KNR-VGES   ! STORCH   *
      * APA01101!23.02.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA02908!17.02.20!2.5 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1189                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1189.
          05 PI-DRPA1189-DATEN.
170220       10 PI-DRPA1189-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA1189-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1189-PVER-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1189.
          05 PO-DRPA1189-RC                         PIC  X(003).
             88 PO-DRPA1189-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1189-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1189-DATEN.
             10 PO-DRPA1189-PVER-VNR                PIC  X(015).
             10 PO-DRPA1189-PVER-KUNDE-NAME         PIC  X(025).
             10 PO-DRPA1189-PVER-KUNDE-VNAME        PIC  X(020).
             10 PO-DRPA1189-PVER-AKT-PIN            PIC S9(009) BINARY.
             10 PO-DRPA1189-PVER-VORVERTRAG         PIC  X(001).
             10 PO-DRPA1189-KNR-MANDANT             PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

