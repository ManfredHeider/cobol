      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0267                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PRODUKT ERMITTELN   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.06.97! NEU                            ! WOLFF    *
      * APA00225!21.03.03! ERWEITERUNG PG-TARIF           ! STORCH   *
      * APA00712!07.04.08! ERWEITERUNG PI-BEREICH         ! STORCH   *
      * DSY04734!21.06.22! ERWEITERUNG PI-BEREICH         ! STORCH   *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0267                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0267.
          05 PI-DRPA0267-DATEN.
             10 PI-DRPA0267-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA0267-TARIF            PIC  X(015).
             10 PI-DRPA0267-STICHTAG         PIC  X(010).
             10 PI-DRPA0267-KNR-MANDANT      PIC S9(004)  BINARY.
210622       10 PI-DRPA0267-S-VSTAT-SPEICH   PIC  X(001).      
      *
       01 PO-DRPA0267.
          05 PO-DRPA0267-RC                  PIC  X(003).
             88 PO-DRPA0267-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0267-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA0267-DATEN.
             10 PO-DRPA0267-PROD-PROD-SCHL   PIC S9(004)  BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************