      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0999.CPY  $
      *    $Revision:   1.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA0999.CPv  $
      *
      *   Rev 1.9   Apr 11 2017 08:31:26   t13ae37
      *APA02353: Neuer Produktpartner TeleSon 
      *
      *   Rev 1.8   Feb 23 2016 08:52:16   t13ae37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 1.7   Feb 03 2016 14:58:22   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 1.6   Mar 10 2014 16:48:12   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 1.5   Jul 26 2013 11:42:56   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 1.4   Aug 16 2010 10:05:26   T13EX44
      *APA01080: Erweiterung AVS-DAISY
      *
      *   Rev 1.3   Dec 10 2007 11:25:34   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 1.2   Oct 19 2005 12:02:00   t13ex44
      *APA00388: Erweiterung SA815 (Satzlänge/3-stellige Prod-Schlü
      *ssel)
      *
      *   Rev 1.1   May 27 1999 11:48:58   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   01 Apr 1998 14:22:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0999  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: ERB / JÄGER-SOKOLUK                             *
      *    DATUM:    07.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.07.98! 01 ! NEUERSTELLUNG             ! JÄGER    *
      * APA00012!31.08.98! 02 ! VERGRÖßERUNG WG. NEUEM    ! JÄGER    *
      *         !        !    ! ATTRIBUT                  !          *
      * APA00012!02.09.98! 03 ! VERKLEINERUNG WG. VARCHAR-! JÄGER    *
      *         !        !    ! BEREICH                   !          *
      * AFPA0012!28.10.98! 04 ! VERGRÖßERUNG WG. NEUEM    ! WOLFF    *
      *         !        !    ! ATTRIBUT PDTA-ID          !          *
      * APA00388!04.10.05!1.2 ! LAENGE ERWEITERT          ! SCHEIDEL *
      * APA00622!20.09.07!1.3 ! LAENGE ERWEITERT          ! STORCH   *
      * APA01080!03.08.10!1.4 ! LAENGE ERWEITERT          ! SCHEIDEL *
260713* APA01539!26.07.13!1.5 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA02208!29.01.16!1.6 ! ERWEITERUNG FÜR LUCY      ! BUCH     *
      * APA02208!23.02.16!1.7 ! Korrektur                 ! BUCH     *
      * APA02353!10.04.17!1.8 ! TELESON PMBE-PG-RECH-NR   ! BUCH     *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0999                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0999.
          05 PI-DRPA0999-UFKT               PIC  9(002).

140921    05 PI-DRPA0999-DATEN              PIC  X(2160).
      *
       01 PO-DRPA0999.
          05 PO-DRPA0999-RC                 PIC  X(003).
             88 PO-DRPA0999-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************
 
 

