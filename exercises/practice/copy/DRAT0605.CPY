      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0605.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0605.CPv  $
      *
      *   Rev 2.3   Feb 04 2016 14:08:00   t13ex78
      *AKI01338: Wartungsauftrag für Abgleich TBAT0042 - Sysin Memb
      *er - Erneute Korrektur
      *
      *   Rev 2.2   Feb 04 2016 13:19:46   t13ex78
      *AKI01338: Wartungsauftrag für Abgleich TBAT0042 - Sysin Memb
      *er - Erneute Korrektur
      *
      *   Rev 2.1   Feb 04 2016 10:33:54   t13ex78
      *AKI01338: Wartungsauftrag für Abgleich TBAT0042 - Sysin Memb
      *er - neue Abfragelogik inkl. neuer Meldungen
      *
      *   Rev 2.0   Feb 03 2016 10:14:40   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0605                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0605                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01338!03.02.16!    ! Neu                       ! tfl      *
      * -------------------------------------------------------------*
      * AKI01338!04.02.16!    ! Select umgestrickt        ! tfl      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*  
      * DBO9152 !10.09.21!    ! Anzahl Stunden in DRAT0605! t13ex78  *
      *         !        !    ! für die gesucht werden    !          *
      *         !        !    ! wird nun über das PIPO ver!          *
      *         !        !    ! sorgt                     !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0605
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0605.
           05  PI-DRAT0605-KEY.
 
             10 PI-DRAT0605-ABKJOBNAME        PIC X(8).
             10 PI-DRAT0605-ABKSTUNDEN        PIC 9(02).
 
       01  PO-DRAT0605.
           05  PO-DRAT0605-RC                 PIC  X(003).
               88 PO-DRAT0605-RC-OK           VALUE 'OK '.
               88 PO-DRAT0605-RC-NOT-FOUND    VALUE 'NFD'.
 
040216     05  PO-DRAT0605-DATEN.
040216       10 PO-DRAT0605-ABKBEGABLAUF      PIC X(26).
040216       10 PO-DRAT0605-ABKBEGAKTU        PIC X(26).
040216       10 PO-DRAT0605-ABKBEGEINARB      PIC X(26).
040216       10 PO-DRAT0605-ABKENDAKTU        PIC X(26).
040216       10 PO-DRAT0605-ABKENDEINARB      PIC X(26).
040216       10 PO-DRAT0605-ABKKATALOGDATUM   PIC X(10).
040216       10 PO-DRAT0605-ABKPARTNERDATUM   PIC X(10).
 
 

