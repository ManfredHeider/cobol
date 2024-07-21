      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0565.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0565.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:03:14   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 11:55:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0565                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMPR BEI EVS-RÜCKB. >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * afpa0012!19.05.98! 01 ! rc-mfe hinzu              ! parent   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0565                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0565.
          05 PI-DRPA0565-DATEN.
             10 PI-DRPA0565-FAKTOR                  PIC S9(001)V9(6)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0565-PMPR.
                COPY DRPA0412.
             10 PI-DRPA0565-PMBE.
                COPY DRPA0411.
      *
       01 PO-DRPA0565.
          05 PO-DRPA0565-RC                         PIC  X(003).
             88 PO-DRPA0565-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0565-RC-VSTAT-MIT-FEHL             VALUE 'VMF'.
             88 PO-DRPA0565-RC-VSTAT-OHNE-FEHL            VALUE 'VOF'.
             88 PO-DRPA0565-RC-MELDUNGSFEHLER             VALUE 'MFE'.
          05 PO-DRPA0565-DATEN.
             10 PO-DRPA0565-PMPR.
                COPY DRPA0412.
      *
      ******* ENDE COPY-MEMBER **************************************
