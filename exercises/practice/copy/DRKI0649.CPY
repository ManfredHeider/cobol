      ****************************************************************
      *    COPY-MEMBER: DRKI0649  FUNKTIONEN LIST-OZP                *
      *                           GUTHABENANTEIL                     *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                         *
      *    DATUM:    28.10.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00043!28.10.99!      NEUERSTELLUNG             ! PHILIPP  *
      * AKI00327!02.06.03!      neue Fkt. DRKI1555        ! R. Biehl *
      * AKI00892!20.10.10!      neue Fkt. DRKI2430        ! who      *
      *DBO-10080!12.08.22!Fkt. DRKI0552 entfernt          !u131774   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXKI0313.
           05 PI-ZXKI0313-FUNKTION          PIC X(008).
              88 PI-ZXKI0313-DRKI0650          VALUE 'DRKI0650'.
      *       88 PI-ZXKI0313-DRKI0552-LIST     VALUE 'DRKI0552'.
              88 PI-ZXKI0313-DRKI0959-LIST     VALUE 'DRKI0959'.
020603        88 PI-ZXKI0313-DRKI1555-LIST     VALUE 'DRKI1555'.
201010        88 PI-ZXKI0313-DRKI2430-LIST     VALUE 'DRKI2430'.
      *---------------------------------------------------------------*
