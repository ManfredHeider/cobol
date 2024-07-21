      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0963.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI0963.CPv  $
      *
      *   Rev 2.7   Sep 09 2015 08:47:54   i13az63
      *AAS01137: KLZ-Auswertung mit OZPs
      *
      *   Rev 2.6   Sep 08 2015 19:51:08   i13az63
      *AAS01137: KLZ-Auswertung mit OZPs
      *
      *   Rev 2.5   Sep 08 2015 15:06:10   i13az63
      *AAS01137: KLZ-Auswertung mit OZPs
      *
      *   Rev 2.4   Sep 18 2012 15:13:34   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.3   Jan 08 2003 10:52:38   t13ex27
      *AKI0299 - Änderung VSA-Statistik
      *
      *   Rev 2.2   18 Sep 2000 08:54:46   T13AE21
      *AKI00138
      *
      *   Rev 2.1   09 Sep 2000 07:31:28   T13EX39
      *AKI00068 NEU
      *
      *   Rev 2.1   Jul 27 1999 14:52:02   T13EX41
      *AKI00036
      *- NEUERSTELLUNG
      *
      *   Rev 2.0   Apr 29 1999 13:57:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0963                                     *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: C. HEINECK                                      *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!10.10.00! 01 !NEU                        ! WHO      *
      * -------------------------------------------------------------*
      * AKI00138!11.09.00!    !Funktionsaufruf            !          *
      * -------------------------------------------------------------*
      * AKI00299!26.08.02!2.3 !Neue Funktion              !I.Frenzel *
      * -------------------------------------------------------------*
      * AKI00886!15.11.11!2.4 !Neue Kundenaktualisierung  !I.Frenzel *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0963                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VSA-RELEVANTE DATEN AUFLISTEN                   *
      *                                                              *
      ****************************************************************
       01  PI-ZXKI0067.
           05 PI-ZXKI0067-FUNKTION            PIC X(008).
080915        88 PI-ZXKI0067-DRKI0964-LIST        VALUE 'DRKI0964'.
110900        88 PI-ZXKI0067-DRKI1124-LIST        VALUE 'DRKI1124'.
260802        88 PI-ZXKI0067-DRKI1438-LIST        VALUE 'DRKI1438'.
      ******** Ende Copy-Member **************************************
 
 

