      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1046.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1046.CPv  $
      *
      *   Rev 2.5   Sep 26 2016 11:18:00   t13ex67
      *APA02280: Dialoganpassungen PF11-PF14
      *
      *   Rev 2.4   Jul 16 2015 15:09:02   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.3   Jun 19 2012 09:27:02   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:51:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 11:48:46   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 11 1999 09:42:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:48   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1046                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1046                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !18.08.98!    ! NEU                       ! D.WILHELM*
      * APA00622!27.07.07!2.01! NEUES EINGABEFELD KNR-VGES! STORCH   *
      * APA01101!23.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01932!06.05.15!2.04! PO-DRPA1046-PG-UST-BETRAG ! WANG     *
      *         !        !    ! NEU                       !          *
      * ????    !20.06.16!2.05! NEUES SCHLÜSSELFELD       ! STORCH   *
      *         !        !    ! PRODUKT                   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1046
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA1046.
           05  PI-DRPA1046-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST NEXT, 3=QUIT
      *
           05  PI-DRPA1046-AUSWAHL-KRITERIEN.
               10 PI-DRPA1046-PGEB-GES         PIC  9(02).
               10 PI-DRPA1046-S-PGEB-GES       PIC  X(01).
               10 PI-DRPA1046-ABR-DAT-VON      PIC  X(10).
               10 PI-DRPA1046-S-ABR-DAT-VON    PIC  X(01).
               10 PI-DRPA1046-ABR-DAT-BIS      PIC  X(10).
               10 PI-DRPA1046-S-ABR-DAT-BIS    PIC  X(01).
               10 PI-DRPA1046-MANDANT          PIC  9(02).
               10 PI-DRPA1046-S-MANDANT        PIC  X(01).
200616         10 PI-DRPA1046-PRODUKT          PIC  9(04).
200616         10 PI-DRPA1046-S-PRODUKT        PIC  X(01).
      *
       01  PO-DRPA1046.
           05  PO-DRPA1046-RC                 PIC  X(003).
               88 PO-DRPA1046-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1046-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1046-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA1046-ERGEBNISZEILE.
               10 PO-DRPA1046-PGEB-GES         PIC  99.
               10 PO-DRPA1046-PROV-SUMME       PIC  S9(09)V99 COMP-3.
               10 PO-DRPA1046-ANZ-FEHL         PIC  S9(07)    COMP-3.
               10 PO-DRPA1046-ANZ-ZUKUNFT      PIC  S9(07)    COMP-3.
               10 PO-DRPA1046-ABR-DAT-VON      PIC  X(10).
               10 PO-DRPA1046-ABR-DAT-BIS      PIC  X(10).
060515         10 PO-DRPA1046-PG-UST-BETRAG    PIC  S9(09)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


